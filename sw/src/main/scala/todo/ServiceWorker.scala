package todo

import org.scalajs.dom.experimental.Fetch._
import org.scalajs.dom.ServiceWorkerGlobalScope
import org.scalajs.dom.ServiceWorkerGlobalScope.self
import org.scalajs.dom.experimental.serviceworkers.{
  ExtendableEvent,
  FetchEvent,
}
import org.scalajs.dom.experimental._
import org.scalajs.dom.raw.MessageEvent

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.timers.*
import scala.concurrent.duration.*

object ServiceWorker {
  val busCache = "cb-bus"
  val pointless = "does this make the sw js file change?"
  assert(pointless != null)
  
  // Notification state
  private var notificationInterval: Option[SetIntervalHandle] = None
  private var currentPlan: Option[js.Dynamic] = None
  private var notificationsEnabled: Boolean = false

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
      (event: MessageEvent) => {
        val data = event.data.asInstanceOf[js.Dynamic]
        val action = data.action.asInstanceOf[String]
        
        action match {
          case "START_NOTIFICATIONS" =>
            notificationsEnabled = true
            currentPlan = Some(data.plan)
            startNotificationTimer()
            // Send acknowledgment back
            val ports = event.ports.asInstanceOf[js.Array[js.Dynamic]]
            if (ports.length > 0) {
              ports(0).postMessage(js.Dynamic.literal(status = "started"))
            }
            
          case "STOP_NOTIFICATIONS" =>
            notificationsEnabled = false
            stopNotificationTimer()
            // Send acknowledgment back
            val ports = event.ports.asInstanceOf[js.Array[js.Dynamic]]
            if (ports.length > 0) {
              ports(0).postMessage(js.Dynamic.literal(status = "stopped"))
            }
            
          case "UPDATE_PLAN" =>
            currentPlan = Some(data.plan)
            if (notificationsEnabled) {
              updateNotification()
            }
            
          case _ =>
            println(s"Unknown action: $action")
        }
      },
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
  
  private def startNotificationTimer(): Unit = {
    stopNotificationTimer()
    updateNotification()
    
    // Update every 30 seconds
    notificationInterval = Some(setInterval(30.seconds) {
      updateNotification()
    })
  }
  
  private def stopNotificationTimer(): Unit = {
    notificationInterval.foreach(clearInterval)
    notificationInterval = None
  }
  
  private def updateNotification(): Unit = {
    currentPlan.foreach { planData =>
      val segments = planData.routeSegments.asInstanceOf[js.Array[js.Dynamic]]
      
      // Find next segment
      val now = new js.Date().getTime()
      val nextSegmentOpt = segments.find { segment =>
        val startTime = parseTime(segment.s.t.asInstanceOf[js.Dynamic])
        startTime > now
      }
      
      nextSegmentOpt.foreach { segment =>
        val startTime = parseTime(segment.s.t.asInstanceOf[js.Dynamic])
        val routeName = segment.route.userFriendlyName.asInstanceOf[String]
        val minutesUntil = Math.max(0, ((startTime - now) / 60000).toLong)
        
        showNotification(minutesUntil, routeName)
      }
    }
  }
  
  private def parseTime(timeObj: js.Dynamic): Double = {
    // Parse the time object - this will need to match your actual time format
    val minutes = timeObj.localTime.value.asInstanceOf[Int]
    val today = new js.Date()
    today.setHours(minutes / 60)
    today.setMinutes(minutes % 60)
    today.setSeconds(0)
    today.getTime()
  }
  
  private def showNotification(minutesUntil: Long, routeName: String): Unit = {
    val message = if (minutesUntil <= 0) {
      s"$routeName route starting now!"
    } else if (minutesUntil == 1) {
      s"$routeName route starts in 1 minute"
    } else {
      s"$routeName route starts in $minutesUntil minutes"
    }
    
    // Use the service worker registration to show notification
    val options = js.Dynamic.literal(
      body = message,
      icon = "/images/BILLDING_LogoMark-256.png",
      badge = "/images/BILLDING_LogoMark-256.png",
      tag = "route-countdown",
      requireInteraction = false,
      silent = true,
      renotify = false
    )
    
    // Call showNotification dynamically
    self.registration.asInstanceOf[js.Dynamic].showNotification("Route Countdown", options)
  }

}
