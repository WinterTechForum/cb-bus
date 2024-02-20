package crestedbutte

import com.billding.time.{TimePicker, WallTime}
import crestedbutte.Browser.Browser
import crestedbutte.laminar.{AppMode, Components}
import org.scalajs.dom
import org.scalajs.dom.experimental.serviceworkers._
import org.scalajs.dom.ServiceWorkerRegistrationOptions
import urldsl.errors.DummyError
import urldsl.language.QueryParameters
import zio.{ZIO, ZLayer}

import java.time.{OffsetDateTime, ZoneId}
import scala.util.{Failure, Success}
import zio.ZIOAppDefault

object MyApp extends ZIOAppDefault {
  override def run = {
    val myEnvironment =
      ZLayer.succeed(BrowserLive.browser)
    fullApplicationLogic.provide(myEnvironment)
  }

  val fullApplicationLogic =
    for {
      _ <- registerServiceWorker()
      _ <- ZIO.attempt {
        val appHolder = dom.document.getElementById("landing-message")
        appHolder.innerHTML = ""
        com.raquo.laminar.api.L.render(
          appHolder,
          RoutingStuff.app,
        )
      }
    } yield 0

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
