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
        println("Invalidating cache!")
        invalidateCache() // TODO Do I need this at all?
        self.clients.claim()
      },
    )

    self.addEventListener(
      "message",
      (event: MessageEvent) => {},
    )

    self.addEventListener(
      "fetch",
      (event: FetchEvent) =>
        event.respondWith(
          fromCache(event.request).recoverWith { case error =>
            fetch(event.request).toFuture
          }.toJSPromise,
        ),
    )

    println("main: ServiceWorker installing...")
  }

  def toCache(): Future[Unit] =
    self.caches
      .flatMap(_.open(busCache).toFuture.flatMap { cache =>
        println("toCache: caching assets...")
        cache.addAll(todoAssets).toFuture
      })
      .get

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

}
