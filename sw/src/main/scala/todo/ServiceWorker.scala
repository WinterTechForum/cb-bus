package todo

import org.scalajs.dom.experimental.Fetch._
import org.scalajs.dom.ServiceWorkerGlobalScope
import org.scalajs.dom.ServiceWorkerGlobalScope.self
import org.scalajs.dom.experimental.serviceworkers.{
  ExtendableEvent,
  FetchEvent,
}
import org.scalajs.dom.experimental._
import org.scalajs.dom.MessageEvent
import crestedbutte.Plan
import crestedbutte.ServiceWorkerAction
import com.billding.time.WallTime
import zio.json.*

import java.time.LocalTime
import java.time.format.DateTimeFormatter

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.timers.*
import scala.concurrent.duration.*
import scala.scalajs.js.JSON
import todo.facades.{ServiceWorkerResponse, ExtendableMessageEvent, WindowClientExtensions}
import WindowClientExtensions._

object ServiceWorker {
  val busCache = "cb-bus"

  // Notification state
  private var notificationInterval: Option[SetIntervalHandle] = None
  private var currentPlan: Option[Plan] =
    None // TODO Convert to strongly typedPlan
  private var notificationsEnabled: Boolean = false

  val todoAssets: js.Array[RequestInfo] =
    if (false) {
      List[RequestInfo](
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
    }
    else {
      List.empty.toJSArray
    }

  def main(
    args: Array[String],
  ): Unit = {
    self.addEventListener(
      "install",
      (event: ExtendableEvent) => {
        self.skipWaiting();
        event.waitUntil(toCache().toJSPromise)
      },
    )
    self.addEventListener(
      "register",
      (event: ExtendableEvent) =>
        event.waitUntil(toCache().toJSPromise),
    )

    self.addEventListener(
      "activate",
      (event: ExtendableEvent) =>
        // Keep existing cache so users can work offline; SW will update in background
        self.clients.claim(),
    )

    self.addEventListener(
      "message",
      (e: MessageEvent) => {
        val event = e.asInstanceOf[ExtendableMessageEvent]
        println("message: ServiceWorker received message: " + event)
        println(
          "JSON.stringify(event.data.toString): " + JSON.stringify(
            event.data.asInstanceOf[js.Dynamic],
          ),
        )
        val action =
          event.data.toString
            .fromJson[ServiceWorkerAction]
            .getOrElse(
              throw new Exception(
                "Error parsing ServiceWorkerAction",
              ),
            )

        action match {
          case ServiceWorkerAction.StartNotifications(plan) =>
            println("Service worker, starting notifications")
            notificationsEnabled = true
            currentPlan = Some(plan)
            println(
              "Service worker, current plan: " +
                plan,
            )
            startNotificationTimer()
            // Send acknowledgment back
            if (event.ports.length > 0) {
              event.ports(0).postMessage(
                ServiceWorkerResponse("started"),
              )
            }

          case ServiceWorkerAction.StopNotifications =>
            notificationsEnabled = false
            stopNotificationTimer()
            // Send acknowledgment back
            if (event.ports.length > 0) {
              event.ports(0).postMessage(
                ServiceWorkerResponse("stopped"),
              )
            }

          case ServiceWorkerAction.UpdatePlan(plan) =>
            currentPlan = Some(plan)
            if (notificationsEnabled) {
              updateNotification()
            }

          case ServiceWorkerAction.TestNotify =>
            // For local testing: show an immediate notification
            currentPlan.foreach(_ => updateNotification())
            if (event.ports.length > 0) {
              event.ports(0).postMessage(
                ServiceWorkerResponse("test-notified"),
              )
            }
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

  }

  def toCache(): Future[Unit] =
    self.caches
      .flatMap(_.open(busCache).toFuture.flatMap { cache =>
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
            val wc = c.asInstanceOf[serviceworkers.WindowClient]
            wc.navigateOption match {
              case Some(navigate) =>
                println(s"Navigating client: ${wc.url}")
                Some(navigate(wc.url).toFuture.map(_ => ()))
              case None =>
                println("Client does not support navigation")
                None
            }
          }
          Future.sequence(reloads).map(_ => ())
        }
  }

  private def startNotificationTimer(): Unit = {
    stopNotificationTimer()
    updateNotification()

    notificationInterval = Some(setInterval(5.seconds) {
      updateNotification()
    })
  }

  private def stopNotificationTimer(): Unit = {
    notificationInterval.foreach(clearInterval)
    notificationInterval = None
  }

  private def updateNotification(): Unit =
    import scala.scalajs.js.JSON
    currentPlan.foreach { planData =>
      println(
        s"SW. updateNotification: updating notification for $planData",
      )
      val segments =
        planData.routeSegments

      // Find next segment
      val now =
        WallTime(
          LocalTime
            .now()
            .format(
              DateTimeFormatter.ofPattern("HH:mm"),
            ),
        )
      val nextSegmentOpt = segments.find { segment =>
        val startTime = segment.s.t
        startTime.isAfter(now)
      }

      nextSegmentOpt.foreach { segment =>
        val startTime =
          segment.s.t
        val routeName =
          segment.route.userFriendlyName.asInstanceOf[String]
        val minutesUntil =
          now.between(startTime).minutes.value

        showNotification(minutesUntil, segment.start.l.name)
      }
    }

  private def showNotification(
    minutesUntil: Long,
    routeName: String,
  ): Unit = {
    val message = if (minutesUntil <= 0) {
      s"$routeName bus leaves now!"
    }
    else if (minutesUntil == 1) {
      s"$routeName bus leaves in 1 minute"
    }
    else {
      s"$routeName bus leaves in $minutesUntil minutes"
    }

    // Use the service worker registration to show notification
    /*
    val options = new org.scalajs.dom.NotificationOptions {
      override var body: js.UndefOr[String] = message
      override var icon: js.UndefOr[String] =
        "/images/BILLDING_LogoMark-256.png"
      // badge = "/images/BILLDING_LogoMark-256.png"
      override var tag: js.UndefOr[String] = "route-countdown"
      // val requireInteraction = false
      // val silent = true
      override var renotify: js.UndefOr[Boolean] = false
    }
     */

    val options = org.scalajs.dom.NotificationOptions(
      body = message,
      icon = "/images/BILLDING_LogoMark-256.png",
      // badge = "/images/BILLDING_LogoMark-256.png"
      tag = "route-countdown",
      // val requireInteraction = false
      silent = true,
      renotify = true,
    )

    // Call showNotification dynamically
    self.registration
      .showNotification("Route Countdown", options)
  }

}
