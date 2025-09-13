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

        case Failure(error) =>
          println(
            s"SWC: registration failed: ${error.getMessage}",
          )
      }(global)

  }

}
