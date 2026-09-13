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
  val busCache = OfflineManifest.cacheName

  // Notification state
  private var notificationInterval: Option[SetIntervalHandle] = None
  private var currentPlan: Option[Plan] = None
  private var notificationsEnabled: Boolean = false
  // Track the last notification message to avoid redundant updates
  private var lastNotificationMessage: Option[String] = None

  val todoAssets: js.Array[RequestInfo] =
    OfflineManifest.assets.map(path =>
      js.Dynamic.newInstance(js.Dynamic.global.Request)(path, js.Dynamic.literal(cache = "reload"))
        .asInstanceOf[RequestInfo]
    ).toJSArray

  def main(
    args: Array[String],
  ): Unit = {
    self.addEventListener(
      "install",
      (event: ExtendableEvent) => {
        event.waitUntil(toCache().toJSPromise)
      },
    )
    self.addEventListener(
      "activate",
      (event: ExtendableEvent) =>
        event.waitUntil(self.clients.claim().toFuture.flatMap { _ =>
          val caches = self.caches.get
          caches.keys().toFuture.flatMap { names =>
            // Retain the current release and one previous shell, never trip data.
            val previous = names.toSeq.filter(n => n.startsWith("cb-bus-shell-") && n != busCache).lastOption
            val obsolete = names.toSeq.filter(n => n == "cb-bus" ||
              (n.startsWith("cb-bus-shell-") && n != busCache && !previous.contains(n)))
            Future.sequence(obsolete.map(n => caches.delete(n).toFuture)).map(_ => ())
          }
        }.toJSPromise),
    )

    self.addEventListener(
      "message",
      (event: MessageEvent) => {
        if (event.data.toString == "ACTIVATE_UPDATE") {
          self.skipWaiting()
        } else if (event.data.toString == "OFFLINE_STATUS") {
          val work = self.caches.get.open(busCache).toFuture.flatMap { cache =>
            Future.sequence(OfflineManifest.assets.map(path =>
              cache.`match`(path).toFuture.map {
                case _: Response => true
                case _ => false
              }
            )).map { available =>
              val ports = event.ports.asInstanceOf[js.Array[js.Dynamic]]
              if (ports.length > 0)
                ports(0).postMessage(js.Dynamic.literal(ready = available.forall(identity)))
            }
          }
          event.asInstanceOf[ExtendableEvent].waitUntil(work.toJSPromise)
        } else {
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
              showDepartureNotification()
            }

          case ServiceWorkerAction.TestNotify =>
            // For local testing: show an immediate notification
            lastNotificationMessage = None
            currentPlan.foreach(_ => showDepartureNotification())
            val ports = event.ports.asInstanceOf[js.Array[js.Dynamic]]
            if (ports.length > 0) {
              ports(0).postMessage(
                js.Dynamic.literal(status = "test-notified"),
              )
            }
        }
        }
      },
    )

    self.addEventListener(
      "fetch",
      (event: FetchEvent) => {
        val request = event.request
        val url = new java.net.URI(request.url)
        val origin = new java.net.URI(self.location.href)
        val sameOrigin = url.getScheme == origin.getScheme && url.getAuthority == origin.getAuthority
        if (request.method.toString == "GET" && sameOrigin) {
          // Query strings carry shared plans; every app URL uses the same shell.
          val path = url.getPath
          val key = if (path == "/" || path == "/index.html") "/index.html" else path
          if (OfflineManifest.assets.contains(key)) {
            event.respondWith(self.caches.get.open(busCache).toFuture.flatMap { cache =>
              cache.`match`(key).toFuture.flatMap {
                case cached: Response => Future.successful(cached)
                case _ => fetch(request).toFuture
              }
            }.toJSPromise)
          }
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

  private def startNotificationTimer(): Unit = {
    stopNotificationTimer()
    // Show a single notification with the departure time
    // We don't try to update it because notification tag replacement
    // doesn't work reliably on Firefox mobile or Safari iOS
    showDepartureNotification()
  }

  private def stopNotificationTimer(): Unit = {
    notificationInterval.foreach(clearInterval)
    notificationInterval = None
    closeExistingNotifications()
  }

  private def closeExistingNotifications(): Unit =
    self.registration
      .getNotifications()
      .toFuture
      .foreach { notifications =>
        notifications.foreach(_.close())
      }

  private def showDepartureNotification(): Unit =
    currentPlan.foreach { planData =>
      val segments = planData.routeSegments

      val now =
        WallTime(
          LocalTime
            .now(java.time.ZoneId.of("America/Denver"))
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
          val departureTime = segment.s.t
          val stopName = segment.start.l.name

          // Show notification with actual departure time only (no countdown)
          // This avoids misleading users since notification updates don't work cross-browser
          val departureTimeStr = departureTime.toDumbAmericanString
          val message =
            s"$departureTimeStr - Bus departs from $stopName"

          val options = org.scalajs.dom.NotificationOptions(
            body = message,
            icon = "/images/BILLDING_LogoMark-256.png",
            tag = "bus-departure",
            silent = false,
            renotify = true,
          )

          self.registration.showNotification("Bus Reminder", options)

        case None =>
          // No upcoming segments
          notificationsEnabled = false
      }
    }

}
