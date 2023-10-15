package todo

import org.scalajs.dom.experimental.Fetch.*
import org.scalajs.dom.{NotificationOptions, ServiceWorkerGlobalScope}
import org.scalajs.dom.ServiceWorkerGlobalScope.self
import org.scalajs.dom.experimental.serviceworkers.{ExtendableEvent, FetchEvent}
import org.scalajs.dom.experimental.*
import org.scalajs.dom.raw.MessageEvent
//import org.scalajs.dom.window.navigator

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.util.{Failure, Success}

object ServiceWorker {
  val busCache = "cb-bus"

  val todoAssets: js.Array[RequestInfo] = List[RequestInfo](
    "/",
    "/index.html",
//    "/index_dev.html",
    "/manifest.webmanifest",
//    "/sw-opt.js",

    // TODO Restore these sometime soon?
//    "/compiledJavascript/main.js",
//    "/compiledJavascript/main.js.map",

    "/favicon.ico",
    "/images/BILLDING_LogoMark-256.png",
    "/styling/style.css",
    "/glyphicons/svg/individual-svg/glyphicons-basic-32-bus.svg",
    "/glyphicons/svg/individual-svg/glyphicons-basic-592-map.svg",
    "/styling/popup_nojs.css",
    "/styling/bulma.min.css",
  ).toJSArray

  def main(args: Array[String]): Unit = {
    self.addEventListener(
      "install",
      (event: ExtendableEvent) => {
        println(
          s"install: service worker with message handler installed > ${event.toString}"
        )
        event.waitUntil(toCache().toJSPromise)
      }
    )

    self.addEventListener(
      "activate",
      (event: ExtendableEvent) => {
        println(
          s"activate: service worker activated > ${event.toString}"
        )
        println("Invalidating cache")
        invalidateCache() // TODO Do I need this at all?
//      event.waitUntil(toCache().toJSPromise)
        self.clients.claim()
      }
    )


    self.addEventListener(
      "message",
      (event: MessageEvent) => {

//        navigator.serviceWorker.getRegistration().`then`(registration => {
//          registration.toOption.get.active.postMessage("Hello from the service worker!")
//        }
//        )

        //        self.registration
        //        self.registration.showNotification(
        //          s"This is a notification from the service worker!",
        //          NotificationOptions(
        //            vibrate = js.Array(100d)
        //          )
        //        )
      }
    )


    self.addEventListener(
      "fetch",
      (event: FetchEvent) => {
//        println("Fetching: " + event.request.url)
        event.respondWith(
          fromCache(event.request)
            .recoverWith{ case error => fetch(event.request).toFuture}
            .toJSPromise
        )

        /*
        if (event.request.cache == RequestCache.`only-if-cached`
            && event.request.mode != RequestMode.`same-origin`) {
          println(
            s"fetch: Bug [823392] cache === only-if-cached && mode !== same-orgin' > ${event.request.url}"
          )
        } else {
          fromCache(event.request).onComplete {
            case Success(response) =>
              println(s"fetch: in cache > ${event.request.url}")
              response
            case Failure(error) =>
              println(
                s"fetch: not in cache, calling server... > ${event.request.url} > ${error.printStackTrace()}"
              )
              fetch(event.request).toFuture
                .onComplete {
                  case Success(response) => response
                  case Failure(finalError) =>
                    println(
                      s"fetch: final fetch failed > ${finalError.printStackTrace()}"
                    )
                }
          }
          3
        }

         */
      }
    )

    println("main: ServiceWorker installing...")
  }

  def toCache(): Future[Unit] = {
    self.caches
      .flatMap(_.open(busCache).toFuture.flatMap{ cache =>
        println("toCache: caching assets...")
        cache.addAll(todoAssets).toFuture})
  }.get

  def fromCache(request: Request): Future[Response] =
    self.caches
      .map(_.`match`(request)
      .toFuture
        .flatMap {
          case response: Response =>
//            if ( request.url.contains("index"))
//            println(s"fromCache: matched request > ${request.url}")
            Future.successful(response)
          case other =>
            println(s"fromCache: missed request > ${request.url}")
            Future.failed(new Exception("Could not find cached request"))
        }
      ).get

  def invalidateCache(): Unit = {
    self.caches
      .map(_.delete(busCache)
      .toFuture
      .map { invalidatedCache =>
        if (invalidatedCache) {
//          println(
//            s"invalidateCache: cache invalidated!', $invalidatedCache"
//          )
          toCache()
        }
      }
    ).get
  }
}
