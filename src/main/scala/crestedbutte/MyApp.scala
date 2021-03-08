package crestedbutte

import com.billding.time.{BusTime, ColoradoClock, TurboClock}
import crestedbutte.Browser.Browser
import crestedbutte.laminar._
import crestedbutte.routes._
import org.scalajs.dom
import org.scalajs.dom.experimental.serviceworkers._
import zio.clock._
import zio.console.Console
import zio.{App, ZIO, ZLayer}

import java.time.{OffsetDateTime, ZoneId}
import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Success}

object MyApp extends App {

  override def run(
    args: List[String],
  ): ZIO[zio.ZEnv, Nothing, zio.ExitCode] = {
    val myEnvironment =
      ZLayer.succeed(BrowserLive.browser) ++ Console.live ++
      ZLayer.succeed(ColoradoClock.live)

    fullApplicationLogic.provideLayer(myEnvironment).exitCode
  }

  def getOptional[T](
    parameterName: String,
    typer: String => Option[T],
  ): ZIO[Browser, Nothing, Option[T]] =
    ZIO
      .access[Browser](_.get)
      .map(
        browser =>
          UrlParsing
            .getUrlParameter(
              browser.window().location.toString,
              parameterName,
            )
            .flatMap(typer),
      )

  import com.raquo.laminar.api.L._

  val fullApplicationLogic =
    for {
      clockParam: Clock.Service <- ZIO.access[Clock](_.get)
      pageMode: AppMode.Value <- getOptional("mode",
                                             AppMode.fromString)
        .map(
          _.getOrElse(AppMode.Production),
        )
      fixedTime <- getOptional("time", x => Some(BusTime(x)))

      clock = if (fixedTime.isDefined)
        TurboClock.TurboClock(
          s"2020-02-21T${fixedTime.get.toString}:00.00-07:00",
        )
      else clockParam

      javaClock = if (fixedTime.isDefined)
        java.time.Clock.fixed(
          OffsetDateTime
            .parse(
              s"2020-02-21T${fixedTime.get.toString}:00.00-07:00",
            )
            .toInstant,
          ZoneId.of("America/Denver"),
        )
      else
        java.time.Clock.system(ZoneId.of("America/Denver"))
      _ <- registerServiceWorker()
      _ <- ZIO {
        val initialRouteOpt: Option[String] =
          UrlParsing
            .getUrlParameter(
              dom.window.location.toString,
              "route",
            )

        dom.document.getElementById("landing-message").innerHTML = ""
        render(
          dom.document.getElementById("landing-message"),
          TagsOnlyLocal.FullApp(pageMode, initialRouteOpt, javaClock),
        )
      }
    } yield {
      0
    }

  def registerServiceWorker(): ZIO[Browser, Nothing, Unit] =
    ZIO
      .access[Browser](_.get)
      .map {
        browser =>
          // TODO Ew. Try to get this removed after first version of PWA is working
          import scala.concurrent.ExecutionContext.Implicits.global
          println("Attempting to register sw")

          toServiceWorkerNavigator(browser.window().navigator).serviceWorker
            .register("./sw-opt.js")
            .toFuture
            .onComplete {
              case Success(registration) =>
                registration.update()
              case Failure(error) =>
                println(
                  s"registerServiceWorker: service worker registration failed > ${error.printStackTrace()}",
                )
            }
      }

}
