package crestedbutte

import crestedbutte.Browser.Browser
import crestedbutte.laminar.{AppMode, Components, TouchControls}
import org.scalajs.dom
import org.scalajs.dom.experimental.serviceworkers.*
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
