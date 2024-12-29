package crestedbutte

import org.scalajs.dom
import zio.{ZIO, ZIOAppDefault, ZLayer}

object MyApp extends ZIOAppDefault {
  override def run = {
    val myEnvironment =
      ZLayer.succeed(BrowserLive.browser)
    fullApplicationLogic.provide(myEnvironment)
  }

  val fullApplicationLogic =
    for {
      _ <- ServiceWorkerClient.registerServiceWorker()
      _ <- ZIO.attempt {
        val appHolder = dom.document.getElementById("landing-message")
        appHolder.innerHTML = ""
        com.raquo.laminar.api.L.render(
          appHolder,
          RoutingStuff.app,
        )
      }
    } yield 0

}
