package crestedbutte

import java.util.concurrent.TimeUnit

import com.billding.time.{BusTime, ColoradoClock, TurboClock}
import crestedbutte.dom.BulmaBehaviorLocal
import crestedbutte.routes._
import org.scalajs.dom.experimental.serviceworkers._
import zio.clock._
import zio.console.Console
import zio.duration.Duration
import zio.{App, Has, Schedule, ZIO, ZLayer}
import zio.console._
import crestedbutte.Browser.Browser

import scala.util.{Failure, Success}

object MyApp extends App {

  override def run(
    args: List[String],
  ): ZIO[zio.ZEnv, Nothing, zio.ExitCode] = {
    val myEnvironment =
      ZLayer.succeed(BrowserLive.browser) ++ Console.live ++
      ZLayer.succeed(ColoradoClock.live)
//      Clock.live

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
              _.namedRoute.routeName
                .elementNameMatches(routeNameStringParam),
            ),
        )
        .getOrElse(components.head)

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
      .map(ComponentData) ++:
    Seq(
      ComponentData(
        RtaNorthbound.fullSchedule,
      ),
      ComponentData(
        RtaSouthbound.fullSchedule,
      ),
    )

  val fullApplicationLogic =
    for {
      browser    <- ZIO.access[Browser](_.get)
      console    <- ZIO.access[Console](_.get)
      clockParam <- ZIO.access[Clock](_.get)
      pageMode <- getOptional("mode", AppMode.fromString)
        .map(
          _.getOrElse(AppMode.Production),
        )
      fixedTime <- getOptional("time", x => Some(BusTime(x)))
      clock = if (fixedTime.isDefined)
        ZLayer.succeed(
          TurboClock.TurboClock(
            s"2020-02-20T${fixedTime.get.toString}:00.00-07:00",
          ),
        )
      else ZLayer.succeed(clockParam)
      environmentDependencies = ZLayer.succeed(browser) ++ ZLayer
        .succeed(console) ++ clock
      _ <- registerServiceWorker()
      _ <- NotificationStuff.addNotificationPermissionRequestToButton
      _ <- NotificationStuff.displayNotificationPermission
      _ <- DomManipulation.createAndApplyPageStructure(pageMode,
                                                       components)
      _ <- UnsafeCallbacks.attachMenuBehavior
      loopingLogic: ZIO[Any, Throwable, Unit] = loopLogic(pageMode,
                                                          components)
        .provideLayer(
          environmentDependencies,
        )
      _ <- BulmaBehaviorLocal.addMenuBehavior(
        loopingLogic,
      )
      _ <- loopingLogic
        .repeat(Schedule.spaced(Duration.apply(5, TimeUnit.SECONDS)))
    } yield {
      0
    }

  def updateUpcomingArrivalsForRoute(
    componentData: ComponentData,
    currentlySelectedRoute: ComponentData,
  ) =
    if (componentData == currentlySelectedRoute) {
      for {
        arrivalsAtAllRouteStops <- TimeCalculations
          .getUpComingArrivalsWithFullSchedule(
            componentData.namedRoute,
          )
          .catchAll(failure => throw new RuntimeException("ack!"))
        _ <- DomManipulation.updateUpcomingBusSectionInsideElement(
          componentData.componentName,
          TagsOnlyLocal.structuredSetOfUpcomingArrivals(
            arrivalsAtAllRouteStops,
          ),
        )
      } yield ()
    }
    else {
      DomManipulation.hideUpcomingBusSectionInsideElement(
        componentData.componentName,
      )
    }

  def updateUpcomingArrivalsOnPage(
    selectedRoute: ComponentData,
    components: Seq[ComponentData],
  ) =
    for {
      modalIsOpen <- DomMonitoring.modalIsOpen
      _ <- if (modalIsOpen) ZIO.succeed(List())
      else {
        ZIO.collectAll(
          components.map(
            updateUpcomingArrivalsForRoute(
              _,
              selectedRoute,
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
//              browser
//                .querySelector(
//                  "#" + ElementNames.Notifications.submitMessageToServiceWorker,
//                )
//                .foreach(
//                  _.addEventListener(
//                    "click",
//                    (_: MouseEvent) => {
//                      println(
//                        "submitting message to service worker",
//                      )
//                      registration.active.postMessage(
//                        "Submitting a message to the serviceWorker!",
//                      )
//                    },
//                  ),
//                )
                registration.update()
              case Failure(error) =>
                println(
                  s"registerServiceWorker: service worker registration failed > ${error.printStackTrace()}",
                )
            }
      }
}
