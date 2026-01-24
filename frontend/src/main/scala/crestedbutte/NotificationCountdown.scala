package crestedbutte

import com.billding.time.WallTime
import com.raquo.laminar.api.L._
import org.scalajs.dom
import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import crestedbutte.Plan
import crestedbutte.ServiceWorkerAction
import zio.json.*

object NotificationCountdown {

  /** Request notification permission if not already granted or
    * denied.
    */
  def requestPermissionIfNeeded(): Unit =
    if (dom.Notification.permission == "default") {
      dom.Notification.requestPermission(_ => ())
    }

  /** Check if we have permission to show notifications. */
  def hasPermission: Boolean =
    dom.Notification.permission == "granted"

  /** Check if notifications are supported on this browser. This
    * returns false on Safari iOS where notification updates don't
    * work reliably.
    */
  def isSupported: Boolean =
    BrowserCapabilities.supportsNotificationTagReplacement

  /** Start countdown notifications for the current plan. The service
    * worker will handle showing and updating the notification every
    * minute. The notification will auto-stop when the bus arrives.
    */
  def startCountdownNotifications(
    plan: Plan,
  ): Future[js.Dynamic] =
    sendToServiceWorker(ServiceWorkerAction.StartNotifications(plan))

  /** Stop countdown notifications. */
  def stopCountdownNotifications(): Future[js.Dynamic] =
    sendToServiceWorker(ServiceWorkerAction.StopNotifications)

  /** Update the plan in the service worker (e.g., when the user
    * changes their route).
    */
  def updatePlan(
    plan: Plan,
  ): Future[js.Dynamic] =
    sendToServiceWorker(ServiceWorkerAction.UpdatePlan(plan))

  private def sendToServiceWorker(
    action: ServiceWorkerAction,
  ): Future[js.Dynamic] = {
    val navigator = dom.window.navigator.asInstanceOf[js.Dynamic]

    if (
      !js.isUndefined(
        navigator.serviceWorker,
      ) && navigator.serviceWorker != null
    ) {
      val serviceWorker = navigator.serviceWorker

      serviceWorker.ready
        .asInstanceOf[js.Promise[js.Dynamic]]
        .toFuture
        .flatMap { registration =>
          val messageChannel = new dom.MessageChannel()

          val responsePromise = scala.concurrent.Promise[js.Dynamic]()

          messageChannel.port1.onmessage =
            (event: dom.MessageEvent) =>
              responsePromise.success(
                event.data.asInstanceOf[js.Dynamic],
              )

          registration.active.postMessage(
            action.toJson,
            js.Array(messageChannel.port2),
          )

          responsePromise.future
        }
    }
    else {
      Future.failed(new Exception("Service Worker not available"))
    }
  }
}
