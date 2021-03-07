package crestedbutte

import com.billding.time.{BusTime, ColoradoClock, TurboClock}
import crestedbutte.routes._
import org.scalajs.dom.experimental.serviceworkers._
import zio.clock._
import zio.console.Console
import zio.{App, Schedule, ZIO, ZLayer}
import crestedbutte.Browser.Browser
import crestedbutte.laminar.{
  AppMode,
  Bulma,
  ComponentData,
  ComponentDataRoute,
  ComponentDataTyped,
  LaminarRoundTripCalculator,
  RepeatingElement,
  TagsOnlyLocal,
}
import org.scalajs.dom
import zio.duration.durationInt

import java.time.{Instant, OffsetDateTime, ZoneId}
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
      pageMode <- getOptional("mode", AppMode.fromString)
        .map(
          _.getOrElse(AppMode.Production),
        )
      fixedTime <- getOptional("time", x => Some(BusTime(x)))
      clock = if (fixedTime.isDefined)
        ZLayer.succeed(
          TurboClock.TurboClock(
            s"2020-02-21T${fixedTime.get.toString}:00.00-07:00",
          ),
        )
      else ZLayer.succeed(clockParam)
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
//      _ <- NotificationStuff.displayNotificationPermission
      _ <- ZIO {
        val duration =
          new FiniteDuration(10, scala.concurrent.duration.SECONDS)
        val clockTicks = new EventBus[Int]

        val initialRouteOpt: Option[String] =
          UrlParsing
            .getUrlParameter(
              dom.window.location.toString,
              "route",
            )

        val components = AllRoutes.components(pageMode)

        val selectedRoute: Var[ComponentData] = Var(
          initialRouteOpt
            .flatMap(
              initialRoute =>
                components.find(
                  _.componentName.elementNameMatches(initialRoute),
                ),
            )
            .getOrElse(
              ComponentDataRoute(
                TownShuttleTimes,
              ),
            ),
        )

        val timeStamps: Signal[BusTime] = clockTicks.events.foldLeft(
          new BusTime(
            OffsetDateTime.now(javaClock).toLocalTime,
          ),
        )(
          (oldTime, _) =>
            new BusTime(
              OffsetDateTime.now(javaClock).toLocalTime,
            ),
        )

        dom.document.getElementById("landing-message").innerHTML = ""
        render(
          dom.document.getElementById("landing-message"),
          div(
            Bulma.menu(selectedRoute, components),
            RepeatingElement().repeatWithInterval(
              1,
              duration,
            ) --> clockTicks,
            TagsOnlyLocal
              .overallPageLayout(
                selectedRoute.signal,
                timeStamps,
                pageMode,
              ),
          ),
        )
      }
      _ <- (for {
        // TODO Get this attached within the normal laminar app
        _ <- ModalBehavior.addModalOpenBehavior
        _ <- ModalBehavior.addModalCloseBehavior
      } yield ()).repeat(Schedule.spaced(1.second))
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
