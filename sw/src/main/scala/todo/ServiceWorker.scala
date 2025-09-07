package todo

import org.scalajs.dom.experimental.Fetch.*
import org.scalajs.dom.ServiceWorkerGlobalScope
import org.scalajs.dom.ServiceWorkerGlobalScope.self
import org.scalajs.dom.experimental.serviceworkers.{
  ExtendableEvent,
  FetchEvent,
}
import org.scalajs.dom.experimental.*
import org.scalajs.dom.raw.MessageEvent

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*

object ServiceWorker {
  val busCache = "cb-bus"
  val pointless = "does this make the sw js file change?"
  assert(pointless != null)

  val todoAssets: js.Array[RequestInfo] = List[RequestInfo](
    "/",
    "/index.html",
    "/manifest.webmanifest",
    "/compiledJavascript/main.js",
    "/compiledJavascript/main.js.map",
    "/favicon.ico",
    "/images/BILLDING_LogoMark-256.png",
    "/styling/style.css",
    "/glyphicons/svg/individual-svg/glyphicons-basic-32-bus.svg",
    "/glyphicons/svg/individual-svg/glyphicons-basic-592-map.svg",
    "/styling/popup_nojs.css",
    "/styling/bulma.min.css",
  ).toJSArray

  def main(
    args: Array[String],
  ): Unit = {
    self.addEventListener(
      "install",
      (event: ExtendableEvent) => {
        println(
          s"install: service worker with message handler installed > ${event.toString}",
        )
        event.waitUntil(toCache().toJSPromise)
      },
    )

    self.addEventListener(
      "activate",
      (event: ExtendableEvent) => {
        println(
          s"activate: service worker activated > ${event.toString}",
        )
        // Keep existing cache so users can work offline; SW will update in background
        self.clients.claim()
      },
    )

    self.addEventListener(
      "message",
      (event: MessageEvent) => {},
    )

    self.addEventListener(
      "fetch",
      (event: FetchEvent) => {
        val request = event.request
        if (request.method.toString != "GET") {
          event.respondWith(fetch(request).toFuture.toJSPromise)
        }
        else {
          event.respondWith(
            LLMGenerated.staleWhileRevalidate(event).toJSPromise,
          )
        }
      },
    )

    println("main: ServiceWorker installing!")
  }

  def toCache(): Future[Unit] =
    self.caches
      .flatMap(_.open(busCache).toFuture.flatMap { cache =>
        println("toCache: caching assets...")
        cache.addAll(todoAssets).toFuture
      })
      .getOrElse(throw new Exception("ServiceWorker.toCache failure"))

  def fromCache(
    request: Request,
  ): Future[Response] =
    self.caches
      .map(
        _.`match`(request).toFuture
          .flatMap {
            case response: Response =>
              Future.successful(response)
            case other =>
              println(s"fromCache: missed request > ${request.url}")
              Future.failed(
                new Exception("Could not find cached request"),
              )
          },
      )
      .get

  def invalidateCache(): Unit =
    self.caches
      .map(
        _.delete(busCache).toFuture
          .map { invalidatedCache =>
            if (invalidatedCache) {
              toCache()
            }
          },
      )
      .get

  object LLMGenerated {
    def staleWhileRevalidate(
      event: FetchEvent,
    ): Future[Response] = {
      val request = event.request
      self.caches
        .map(
          _.open(busCache).toFuture.flatMap { cache =>
            cache
              .`match`(request)
              .toFuture
              .flatMap {
                case cached: Response =>
                  val updateF = fetch(request).toFuture
                    .flatMap { networkResp =>
                      if (networkResp != null && networkResp.ok) {
                        val changed = hasUpdated(cached, networkResp)
                        cache
                          .put(request, networkResp.clone())
                          .toFuture
                          .flatMap { _ =>
                            if (changed) reloadAllWindows()
                            else Future.unit
                          }
                      }
                      else Future.unit
                    }
                    .recover { case _ => () }
                  event.waitUntil(updateF.toJSPromise)
                  Future.successful(cached)
                case _ =>
                  fetch(request).toFuture.flatMap { networkResp =>
                    if (networkResp != null && networkResp.ok) {
                      cache
                        .put(request, networkResp.clone())
                        .toFuture
                        .map(_ => networkResp)
                    }
                    else Future.successful(networkResp)
                  }
              }
          },
        )
        .getOrElse(fetch(request).toFuture)
    }

    private def headerValue(
      headers: Headers,
      name: String,
    ): Option[String] = Option(headers.get(name))

    private def hasUpdated(
      cached: Response,
      fresh: Response,
    ): Boolean = {
      val cachedEtag = headerValue(cached.headers, "ETag")
      val freshEtag = headerValue(fresh.headers, "ETag")
      val cachedLm = headerValue(cached.headers, "Last-Modified")
      val freshLm = headerValue(fresh.headers, "Last-Modified")
      (cachedEtag, freshEtag, cachedLm, freshLm) match {
        case (Some(a), Some(b), _, _) if a != b => true
        case (_, _, Some(a), Some(b)) if a != b => true
        case _                                  => false
      }
    }

    private def reloadAllWindows(): Future[Unit] =
      self.clients
        .matchAll()
        .toFuture
        .flatMap { clients =>
          val reloads = clients.toSeq.flatMap { c =>
            val dyn = c.asInstanceOf[js.Dynamic]
            val hasNavigate =
              !js.isUndefined(dyn.selectDynamic("navigate"))
            if (hasNavigate) {
              val wc = c.asInstanceOf[serviceworkers.WindowClient]
              Some(wc.navigate(wc.url).toFuture.map(_ => ()))
            }
            else None
          }
          Future.sequence(reloads).map(_ => ())
        }
  }

}
