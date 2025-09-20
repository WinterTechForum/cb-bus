package crestedbutte

import com.billding.time.WallTime
import com.raquo.laminar.api.L._
import org.scalajs.dom
import org.scalajs.dom.experimental.serviceworkers.toServiceWorkerNavigator
import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.concurrent.Future
import crestedbutte.facades.ServiceWorkerMessageData
import scala.concurrent.ExecutionContext.Implicits.global
import crestedbutte.Plan
import crestedbutte.ServiceWorkerAction
import zio.json.*

object NotificationCountdown {

  def requestPermissionIfNeeded(): Unit =
    if (dom.Notification.permission == "default") {
      dom.Notification.requestPermission(_ => ())
    }

  def hasPermission: Boolean =
    dom.Notification.permission == "granted"

  def startCountdownNotifications(
    $plan: Var[Plan],
    timeStampsSignal: Signal[WallTime],
  ): Unit = {
    // Send message to service worker to start notifications
    sendToServiceWorker(
      ServiceWorkerAction.StartNotifications($plan.now()),
    )
      .foreach { response =>
        println(
          s"Service worker notification started: ${response}",
        )
      }

    // Subscribe to plan changes to update service worker
    $plan.signal.foreach { plan =>
      sendToServiceWorker(ServiceWorkerAction.UpdatePlan(plan))
    }(unsafeWindowOwner)
  }

  def stopCountdownNotifications(): Unit =
    // Send message to service worker to stop notifications
    sendToServiceWorker(ServiceWorkerAction.StopNotifications)
      .foreach { response =>
        println(s"Service worker notification stopped: $response")
      }

  private def sendToServiceWorker(
    action: ServiceWorkerAction,
  ): Future[js.Dynamic] = {
    try {
      val serviceWorker = toServiceWorkerNavigator(dom.window.navigator).serviceWorker
      println("NotificationCountdown: service worker available")

      serviceWorker.ready.toFuture
        .flatMap { registration =>
          val messageChannel = new dom.MessageChannel()

          val responsePromise = scala.concurrent.Promise[js.Dynamic]()

          messageChannel.port1.onmessage =
            (event: dom.MessageEvent) =>
              responsePromise.success(
                event.data.asInstanceOf[js.Dynamic],
              )

          println(
            "NotificationCountdown: sending message to service worker",
          )
          registration.active.postMessage(
            action.toJson,
            js.Array(messageChannel.port2),
          )

          responsePromise.future
        }
    } catch {
      case _: Exception =>
        Future.failed(new Exception("Service Worker not available"))
    }
  }
}
