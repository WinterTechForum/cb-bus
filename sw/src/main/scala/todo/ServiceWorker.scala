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

object ServiceWorker {
  val busCache = "cb-bus"

  // Notification state
  private var notificationInterval: Option[SetIntervalHandle] = None
  private var currentPlan: Option[Plan] = None
  private var notificationsEnabled: Boolean = false
  // Track the last notification message to avoid redundant updates
  private var lastNotificationMessage: Option[String] = None

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
      (event: MessageEvent) => {
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
            notificationsEnabled = true
            currentPlan = Some(plan)
            lastNotificationMessage = None
            startNotificationTimer()
            // Send acknowledgment back
            val ports = event.ports.asInstanceOf[js.Array[js.Dynamic]]
            if (ports.length > 0) {
              ports(0).postMessage(
                js.Dynamic.literal(status = "started"),
              )
            }

          case ServiceWorkerAction.StopNotifications =>
            notificationsEnabled = false
            stopNotificationTimer()
            lastNotificationMessage = None
            // Close any existing notifications
            closeExistingNotifications()
            // Send acknowledgment back
            val ports = event.ports.asInstanceOf[js.Array[js.Dynamic]]
            if (ports.length > 0) {
              ports(0).postMessage(
                js.Dynamic.literal(status = "stopped"),
              )
            }

          case ServiceWorkerAction.UpdatePlan(plan) =>
            currentPlan = Some(plan)
            if (notificationsEnabled) {
              updateNotification()
            }

          case ServiceWorkerAction.TestNotify =>
            // For local testing: show an immediate notification
            lastNotificationMessage = None
            currentPlan.foreach(_ => updateNotification())
            val ports = event.ports.asInstanceOf[js.Array[js.Dynamic]]
            if (ports.length > 0) {
              ports(0).postMessage(
                js.Dynamic.literal(status = "test-notified"),
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

    // Update every 60 seconds (1 minute) - this aligns with the minute-based countdown
    // and reduces the chance of notification update issues across browsers
    notificationInterval = Some(setInterval(60.seconds) {
      updateNotification()
    })
  }

  private def stopNotificationTimer(): Unit = {
    notificationInterval.foreach(clearInterval)
    notificationInterval = None
  }

  private def closeExistingNotifications(): Unit =
    self.registration
      .getNotifications()
      .toFuture
      .foreach { notifications =>
        notifications.foreach(_.close())
      }

  private def updateNotification(): Unit =
    currentPlan.foreach { planData =>
      val segments = planData.routeSegments

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

      nextSegmentOpt match {
        case Some(segment) =>
          val startTime = segment.s.t
          val stopName = segment.start.l.name
          val minutesUntil = now.between(startTime).minutes.value

          if (minutesUntil <= 0) {
            // Bus is leaving now - show final notification and stop
            showNotification(0, stopName)
            // Auto-stop after the bus leaves
            notificationsEnabled = false
            stopNotificationTimer()
            lastNotificationMessage = None
          }
          else {
            // Show/update the countdown notification
            // Only update if the message has changed (minute changed)
            val newMessage = buildMessage(minutesUntil, stopName)
            if (!lastNotificationMessage.contains(newMessage)) {
              showNotification(minutesUntil, stopName)
              lastNotificationMessage = Some(newMessage)
            }
          }

        case None =>
          // No more segments - stop notifications
          notificationsEnabled = false
          stopNotificationTimer()
          closeExistingNotifications()
          lastNotificationMessage = None
      }
    }

  private def buildMessage(
    minutesUntil: Long,
    stopName: String,
  ): String =
    if (minutesUntil <= 0)
      s"$stopName bus leaves now!"
    else if (minutesUntil == 1)
      s"$stopName bus leaves in 1 minute"
    else
      s"$stopName bus leaves in $minutesUntil minutes"

  private def showNotification(
    minutesUntil: Long,
    stopName: String,
  ): Unit = {
    val message = buildMessage(minutesUntil, stopName)

    val options = org.scalajs.dom.NotificationOptions(
      body = message,
      icon = "/images/BILLDING_LogoMark-256.png",
      // Using tag allows Chrome/Firefox/Edge to replace existing notification
      // Note: Safari iOS ignores the tag and creates new notifications
      // We hide the feature on Safari iOS via BrowserCapabilities check
      tag = "bus-countdown",
      // silent=false so the user gets an audio alert on updates
      silent = false,
      // renotify=true tells supported browsers to alert on updates
      // even when the tag matches (Chrome supports this, Firefox partially)
      renotify = true,
    )

    self.registration.showNotification("Bus Alert", options)
  }

}
