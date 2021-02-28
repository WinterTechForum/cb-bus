package crestedbutte

import java.util.concurrent.TimeUnit
import com.billding.time.{
  BusDuration,
  BusTime,
  ColoradoClock,
  TurboClock,
}
import crestedbutte.dom.{BulmaBehaviorLocal, DomManipulation}
import crestedbutte.routes._
import org.scalajs.dom.experimental.serviceworkers._
import zio.clock._
import zio.console.Console
import zio.duration.Duration
import zio.{App, Has, Schedule, ZIO, ZLayer}
import zio.console._
import crestedbutte.Browser.Browser
import crestedbutte.laminar.{
  LaminarRoundTripCalculator,
  RepeatingElement,
}
import crestedbutte.laminar.LaminarRoundTripCalculator.calculatorComponentName
import org.scalajs.dom
import org.scalajs.dom.document
import org.scalajs.dom.window
import org.scalajs.dom.raw.HTMLElement
import typings.materialUiCore.mod.TextField
import typings.materialUiPickers.anon.{
  Format,
  OnChange,
  OpenPicker,
  PickPropsWithChildrenCloc,
  PickerProps,
}

import java.time.{LocalTime, ZoneId}
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

  def loopLogic(
    pageMode: AppMode.Value,
    components: Seq[ComponentData],
  ) =
    for {
      routeNameOpt <- getOptional("route", x => Some(x))
      selectedComponent: ComponentData = routeNameOpt
        .flatMap(
          routeNameStringParam =>
            components.find(
              _.componentName
                .elementNameMatches(routeNameStringParam),
            ),
        )
        .getOrElse(components.head)

      // TODO Figure out proper updates
      _ <- updateUpcomingArrivalsOnPage(selectedComponent, components)
      _ <- NotificationStuff.addAlarmBehaviorToTimes
      _ <- ModalBehavior.addModalOpenBehavior
      _ <- ModalBehavior.addModalCloseBehavior
//      _ <- NotificationStuff.checkSubmittedAlarms
    } yield ()

  val mtnExpressRoutes =
    new CompanyRoutes("Mtn Express",
                      Seq(
                        CovidLoop,
                        TownShuttleTimes,
                        CrystalCastleShuttle,
                        ColumbineLoop,
                        SnodgrassShuttle,
                        ThreeSeasonsTimes,
                      ))

  private val components: Seq[ComponentData] =
    mtnExpressRoutes.routesWithTimes
      .map(ComponentDataRoute) ++:
    Seq(
      ComponentDataRoute(
        RtaNorthbound.fullSchedule,
      ),
      ComponentDataRoute(
        RtaSouthbound.fullSchedule,
      ),
      ComponentDataTyped(
        "RoundTripCalculator",
        LaminarRoundTripCalculator.calculatorComponentName,
      ),
    )

  import com.raquo.laminar.api.L._

  val fullApplicationLogic =
    for {
      browser                   <- ZIO.access[Browser](_.get)
      console                   <- ZIO.access[Console](_.get)
      clockParam: Clock.Service <- ZIO.access[Clock](_.get)
      clockTicks = new EventBus[Int]
      javaClock = java.time.Clock.system(ZoneId.of("America/Denver"))
      arrivalsAtAllRouteStops = TimeCalculations
        .getUpComingArrivalsWithFullScheduleNonZio(
          javaClock,
          TownShuttleTimes,
        )
      upcomingArrivalData: Signal[UpcomingArrivalComponentData] = clockTicks.events
        .foldLeft(
          arrivalsAtAllRouteStops,
        ) {
          case (previousTimes, clockTick) =>
            TimeCalculations
              .getUpComingArrivalsWithFullScheduleNonZio(
                javaClock,
                TownShuttleTimes,
              )
        }
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
      environmentDependencies = ZLayer.succeed(browser) ++ ZLayer
        .succeed(console) ++ clock
      _ <- registerServiceWorker()
      _ <- putStrLn("hi")
//      _ <- NotificationStuff.addNotificationPermissionRequestToButton
//      _ <- NotificationStuff.displayNotificationPermission
      // TODO Restore setup behavior here: DomManipulation.createAndApplyPageStructure(
      _ <- ZIO {
        val duration =
          new FiniteDuration(1, scala.concurrent.duration.SECONDS)
        val clockTicks = new EventBus[Int]

        val $triggerState: Signal[Int] =
          clockTicks.events.foldLeft[Int](0)(
            (lastTick, _) => lastTick + 1,
          )

        val arrivalsAtAllRouteStops = TimeCalculations
          .getUpComingArrivalsWithFullScheduleNonZio(
            javaClock,
            TownShuttleTimes,
          )
        render(
          dom.document.getElementById("landing-message"),
          div(
            RepeatingElement().repeatWithInterval(
              1,
              duration,
            ) --> clockTicks,
            child <-- $triggerState.map(
              ticks => div("ticks: " + ticks),
            ),
            child <-- $triggerState.map(
              _ =>
                div("javaTime: " + LocalTime.now(javaClock).toString),
            ),
            TagsOnlyLocal
              .overallPageLayout(
                javaClock,
                upcomingArrivalData,
                pageMode,
                components,
              ),
          ),
        )
      }
      _ <- putStrLn("hello")
      _ <- UnsafeCallbacks.attachMenuBehavior
      // todo restore for laminar stuff
      _ <- ZIO {
        if (org.scalajs.dom.document.getElementById(
              LaminarRoundTripCalculator.calculatorComponentName.name,
            ) != null)
          LaminarRoundTripCalculator.app()
      }
      loopingLogic: ZIO[Any, Throwable, Unit] = loopLogic(pageMode,
                                                          components)
        .provideLayer(
          environmentDependencies,
        )
      // TODO Restore
//      _ <- BulmaBehaviorLocal.addMenuBehavior(
//        loopingLogic,
//      )
      _ <- loopingLogic
        .repeat(Schedule.spaced(Duration.apply(5, TimeUnit.SECONDS)))
    } yield {
      0
    }

  def updateComponents(
    componentData: ComponentData,
    currentlySelectedRoute: ComponentData,
  ) =
    if (componentData == currentlySelectedRoute) {
      currentlySelectedRoute match {
        case ComponentDataRoute(namedRoute) =>
          for {
            // I should make a Signal[UpcomingArrivalComponentData] that goes into
            // the constructor of the page initially
            arrivalsAtAllRouteStops <- TimeCalculations
              .getUpComingArrivalsWithFullSchedule(
                namedRoute,
              )
              .catchAll(failure => throw new RuntimeException("ack!"))
            _ <- DomManipulation.updateContentInsideElementAndReveal(
              componentData.componentName.name,
              TagsOnlyLocal
                .structuredSetOfUpcomingArrivals(
                  arrivalsAtAllRouteStops,
                )
                .ref,
              "upcoming-buses",
            )
          } yield ()
        case ComponentDataTyped(value, componentName) => {
          for {
            _ <- ZIO {
              show(componentName.name)
            }
          } yield ()
        }
      }
    }
    else {
      DomManipulation.hideElement(
        componentData.componentName.name,
      )
    }

  // a decent library would also have this function
  private def show(
    elementId: String,
  ) = {

    val result =
      document.body
        .querySelector(s"#$elementId")
    result.removeAttribute("style")
//      result
//        .setAttribute(
//          "style",
//          "",
//        )
  }

  def updateUpcomingArrivalsOnPage(
    selectedComponent: ComponentData,
    components: Seq[ComponentData],
  ) =
    for {
      modalIsOpen <- DomMonitoring.modalIsOpen
      _ <- if (modalIsOpen) ZIO.succeed(List())
      else {
        ZIO.collectAll(
          components.map(
            updateComponents(
              _,
              selectedComponent,
            ),
          ),
        )
      }
    } yield ()

  def registerServiceWorker(): ZIO[Browser, Nothing, Unit] =
    ZIO
      .access[Browser](_.get)
      .map {
        browser =>
          // TODO Ew. Try to get this removed after first version of PWA is working
          import scala.concurrent.ExecutionContext.Implicits.global

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
