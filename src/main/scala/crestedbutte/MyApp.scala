package crestedbutte

import java.util.concurrent.TimeUnit

import com.billding.time.{BusTime, ColoradoClock, TurboClock}
import crestedbutte.dom.BulmaBehaviorLocal
import crestedbutte.routes._
import org.scalajs.dom.experimental.serviceworkers._
import org.scalajs.dom.raw.MouseEvent
import zio.ZLayer.NoDeps
import zio.clock._
import zio.console.Console
import zio.duration.Duration
import zio.{App, Has, Schedule, ZIO, ZLayer}

import scala.util.{Failure, Success}

object MyApp extends App {

  override def run(
    args: List[String],
  ): ZIO[zio.ZEnv, Nothing, Int] = {
    val console = ZLayer.succeed(Console.live)
    val clock =
      ZLayer.succeed(ColoradoClock.Live)
    val browser =
      ZLayer.succeed(BrowserLive.browser)

    val myEnvironment
      : ZLayer[Any, Nothing, Has[Clock.Service] with Has[
        Browser.Service,
      ]] =
//      console ++ clock ++ browser ++ zio.random.Random.live ++ zio.system.System.live
      clock ++ browser

    fullApplicationLogic.provideLayer(myEnvironment)
  }

  def getOptional[T](
    parameterName: String,
    typer: String => Option[T],
  ): ZIO[Has[Browser.Service], Nothing, Option[T]] =
    ZIO
      .access[Has[Browser.Service]](_.get)
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
  ): ZIO[Has[Clock.Service] with Has[Browser.Service],
         Nothing,
         Unit] =
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
      _ <- NotificationStuff.checkSubmittedAlarms
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

  val fullApplicationLogic: ZIO[Has[Clock.Service] with Has[
    Browser.Service,
  ], Nothing, Int] =
    for {
      pageMode <- getOptional("mode", AppMode.fromString)
        .map(
          _.getOrElse(AppMode.Production),
        )
      fixedTime <- getOptional("time", x => Some(BusTime(x)))
      environmentDependencies = if (fixedTime.isDefined)
        // TODO make TURBO
        ZLayer.succeed(
          TurboClock.TurboClock(
            s"2020-02-20T${fixedTime.get.toString}:00.00-07:00",
          ),
        ) ++ ZLayer.succeed(
          BrowserLive.browser,
        )
      else
        ZLayer.succeed(ColoradoClock.Live) ++ ZLayer.succeed(
          BrowserLive.browser,
        )
      _ <- DomManipulation.createAndApplyPageStructure(
        pageMode,
        components,
      )
      _ <- UnsafeCallbacks.attachMenuBehavior
      _ <- registerServiceWorker()
      _ <- NotificationStuff.addNotificationPermissionRequestToButton
      _ <- NotificationStuff.displayNotificationPermission
      _ <- BulmaBehaviorLocal.addMenuBehavior(
        loopLogic(pageMode, components)
          .provideLayer(
            environmentDependencies,
          ),
      )
      _ <- loopLogic(pageMode, components)
        .provideLayer(
          environmentDependencies,
        )
        .repeat(Schedule.spaced(Duration.apply(1, TimeUnit.SECONDS)))
    } yield {
      0
    }

  def updateUpcomingArrivalsForRoute(
    componentData: ComponentData,
    currentlySelectedRoute: ComponentData,
  ): ZIO[Has[Browser.Service] with Has[Clock.Service],
         Nothing,
         Unit] =
    componentData match {
      case `currentlySelectedRoute` =>
        for {
          arrivalsAtAllRouteStops <- TimeCalculations
            .getUpComingArrivalsWithFullSchedule(
              componentData.namedRoute,
            )
          _ <- DomManipulation.updateUpcomingBusSectionInsideElement(
            componentData.componentName,
            TagsOnlyLocal.structuredSetOfUpcomingArrivals(
              arrivalsAtAllRouteStops,
            ),
          )
        } yield ()
      case other =>
        DomManipulation.hideUpcomingBusSectionInsideElement(
          componentData.componentName,
        )
    }

  def updateUpcomingArrivalsOnPage(
    selectedRoute: ComponentData,
    components: Seq[ComponentData],
  ): ZIO[Has[Browser.Service] with Has[Clock.Service],
         Nothing,
         Any,
  ] =
    for {
      modalIsOpen <- DomMonitoring.modalIsOpen
    } yield
      if (modalIsOpen) ZIO.succeed()
      else
        ZIO.sequence(
          components.map(
            updateUpcomingArrivalsForRoute(
              _,
              selectedRoute,
            ),
          ),
        )

  def registerServiceWorker()
    : ZIO[Has[Browser.Service], Nothing, Unit] =
    ZIO
      .access[Has[Browser.Service]](_.get)
      .map { browser =>
        // TODO Ew. Try to get this removed after first version of PWA is working
        import scala.concurrent.ExecutionContext.Implicits.global

        toServiceWorkerNavigator(browser.window().navigator).serviceWorker
          .register("./sw-opt.js")
          .toFuture
          .onComplete {
            case Success(registration) =>
              browser
                .querySelector(
                  "#" + ElementNames.Notifications.submitMessageToServiceWorker,
                )
                .foreach(
                  _.addEventListener(
                    "click",
                    (_: MouseEvent) => {
                      println(
                        "submitting message to service worker",
                      )
                      registration.active.postMessage(
                        "Submitting a message to the serviceWorker!",
                      )
                    },
                  ),
                )
              registration.update()
            case Failure(error) =>
              println(
                s"registerServiceWorker: service worker registration failed > ${error.printStackTrace()}",
              )
          }
      }
}
