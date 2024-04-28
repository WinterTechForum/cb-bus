package crestedbutte

import crestedbutte.Browser.Browser
import org.scalajs.dom.ServiceWorkerRegistrationOptions
import org.scalajs.dom.experimental.serviceworkers.toServiceWorkerNavigator
import zio.ZIO

import scala.util.{Failure, Success}

object ServiceWorkerClient {
  def registerServiceWorker(): ZIO[Browser, Nothing, Unit] =
    ZIO
      .service[Browser]
      .map { browser =>
        // TODO Ew. Try to get this removed after first version of PWA is working
        import scala.concurrent.ExecutionContext.Implicits.global

        val serviceWorker = toServiceWorkerNavigator(
          browser.window().navigator,
        ).serviceWorker

        serviceWorker.register(
          "./push/onesignal/OneSignalSDKWorker.js",
          new ServiceWorkerRegistrationOptions {
            scope = "/push/onesignal/myCustomScope"
          },
        )

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
          }

      }


}
