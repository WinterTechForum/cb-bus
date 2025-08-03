package crestedbutte

import org.scalajs.dom.ServiceWorkerRegistrationOptions
import org.scalajs.dom.experimental.serviceworkers.toServiceWorkerNavigator

import scala.util.{Failure, Success}

object ServiceWorkerClient {
  def registerServiceWorker(): Unit = {
    val window = org.scalajs.dom.window

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
    serviceWorker
      .register("./sw-opt.js")
      .toFuture
      .onComplete {
        case Success(registration) =>
          println("Registered SW: " + registration.scope)
          registration.update() // TODO When do I need this?

        case Failure(error) =>
          println(
            s"registerServiceWorker: service worker registration failed > ${error.printStackTrace()}",
          )
      }(global)

  }

}
