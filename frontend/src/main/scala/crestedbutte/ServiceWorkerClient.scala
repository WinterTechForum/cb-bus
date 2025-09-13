package crestedbutte

import org.scalajs.dom.ServiceWorkerRegistrationOptions
import org.scalajs.dom.experimental.serviceworkers.toServiceWorkerNavigator
import org.scalajs.dom
import org.scalajs.dom.Event
import scala.scalajs.js

import scala.util.{Failure, Success}

object ServiceWorkerClient {
  def registerServiceWorker(): Unit = {
    val window = org.scalajs.dom.window
    println("SWC: attempting to register service worker...")

    val navigatorDyn = window.navigator.asInstanceOf[js.Dynamic]
    if (js.isUndefined(navigatorDyn.selectDynamic("serviceWorker"))) {
      println(
        "SWC: service workers unsupported or insecure context; skipping registration",
      )
      return
    }

    val serviceWorker = toServiceWorkerNavigator(
      window.navigator,
    ).serviceWorker

    if (window.hasOwnProperty("OneSignalDeferred")) {
      serviceWorker.register(
        "./push/onesignal/OneSignalSDKWorker.js",
        new ServiceWorkerRegistrationOptions {
          scope = "/push/onesignal/myCustomScope"
        },
      )
    }

    import scala.concurrent.ExecutionContext.Implicits.global
    // Listen for messages from the SW and log them
    serviceWorker.onmessage = (e: dom.MessageEvent) => {
      val data = e.data.asInstanceOf[js.Dynamic]
      val kind = data
        .selectDynamic("kind")
        .asInstanceOf[js.UndefOr[String]]
        .toOption
        .getOrElse("")
      val msg = data
        .selectDynamic("message")
        .asInstanceOf[js.UndefOr[String]]
        .toOption
        .getOrElse(data.toString)
      if (kind == "sw-log") println(s"SWC <- ${msg}")
      else println(s"SWC <- message: ${msg}")
    }

    serviceWorker
      .register("/sw.js",
                new ServiceWorkerRegistrationOptions { scope = "/" },
      )
      .toFuture
      .onComplete {
        case Success(registration) =>
          println(
            "SWC: registered service worker with scope: " + registration.scope,
          )
          // Log install/update state changes for visibility
          registration.onupdatefound = (_: Event) => {
            println("SWC: update found for service worker")
            val installing = registration.installing
            if (installing != null) {
              val swDyn = installing.asInstanceOf[js.Dynamic]
              swDyn.updateDynamic("onstatechange")({ (_: Event) =>
                println(
                  "SWC: installing worker state -> " + installing.state,
                )
              }: js.Function1[Event, Any])
            }
          }
          registration.update()

          // If there's an active controller, log that
          val controller = serviceWorker.controller
          if (controller != null)
            println("SWC: controller present -> " + controller.state)
          serviceWorker.oncontrollerchange =
            (_: Event) => println("SWC: controller changed")

          // On localhost, send a TEST_NOTIFY to verify SW messaging & notifications
          val isLocal =
            window.location.hostname == "localhost" || window.location.hostname == "127.0.0.1"
          if (isLocal) {
            serviceWorker.ready.toFuture.foreach { reg =>
              val mc = new dom.MessageChannel()
              mc.port1.onmessage = (e: dom.MessageEvent) =>
                println(s"SWC <- test ack: ${e.data}")
              val msg = js.Dynamic.literal(action = "TEST_NOTIFY")
              reg.active.postMessage(msg, js.Array(mc.port2))
              println("SWC: sent TEST_NOTIFY to service worker")
            }(global)
          }

        case Failure(error) =>
          println(
            s"SWC: registration failed: ${error.getMessage}",
          )
      }(global)

  }

}
