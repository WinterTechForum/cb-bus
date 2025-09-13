package crestedbutte

import com.billding.time.WallTime
import com.raquo.laminar.api.L._
import org.scalajs.dom
import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import upickle.default._

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
    sendToServiceWorker("START_NOTIFICATIONS", $plan.now()).foreach {
      response =>
        println(
          s"Service worker notification started: ${response.toJson}",
        )
    }

    // Subscribe to plan changes to update service worker
    $plan.signal.foreach { plan =>
      sendToServiceWorker("UPDATE_PLAN", plan)
    }(unsafeWindowOwner)
  }

  def stopCountdownNotifications(): Unit =
    // Send message to service worker to stop notifications
    sendToServiceWorker("STOP_NOTIFICATIONS", null).foreach {
      response =>
        println(s"Service worker notification stopped: $response")
    }

  private def sendToServiceWorker(
    action: String,
    plan: Plan,
  ): Future[js.Dynamic] = {
    val navigator = dom.window.navigator.asInstanceOf[js.Dynamic]

    if (
      !js.isUndefined(
        navigator.serviceWorker,
      ) && navigator.serviceWorker != null
    ) {
      println("NotificationCountdown: service worker available")
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

          val message = if (plan != null) {
            // Convert plan to a simple JS object for the service worker
            val planData = js.Dynamic.literal(
              routeSegments = plan.routeSegments.map { segment =>
                js.Dynamic.literal(
                  s = js.Dynamic.literal(
                    t = js.Dynamic.literal(
                      localTime = js.Dynamic.literal(
                        value = segment.s.t.localTime.value,
                      ),
                    ),
                  ),
                  route = js.Dynamic.literal(
                    userFriendlyName = segment.route.userFriendlyName,
                  ),
                )
              }.toJSArray,
            )

            js.Dynamic.literal(
              action = action,
              plan = planData,
            )
          }
          else {
            js.Dynamic.literal(action = action)
          }

          println(
            "NotificationCountdown: sending message to service worker",
          )
          registration.active.postMessage(
            message,
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
