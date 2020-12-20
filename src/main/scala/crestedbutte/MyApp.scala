package crestedbutte

import java.util.concurrent.TimeUnit

import com.billding.time.{BusTime, ColoradoClock, TurboClock}
import crestedbutte.dom.{BulmaBehaviorLocal, DomManipulation}
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
      _ <- DomManipulation.createAndApplyPageStructure(
        TagsOnlyLocal
          .overallPageLayout(pageMode, components)
          .render,
      )
      _ <- UnsafeCallbacks.attachMenuBehavior
      _ <- ZIO {
        LaminarRoundTripCalculator.app("laminar-app")
      }
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
        _ <- DomManipulation.updateContentInsideElementAndReveal(
          componentData.componentName,
          TagsOnlyLocal
            .structuredSetOfUpcomingArrivals(
              arrivalsAtAllRouteStops,
            )
            .render,
          "upcoming-buses",
        )
      } yield ()
    }
    else {
      DomManipulation.hideElement(
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
                registration.update()
              case Failure(error) =>
                println(
                  s"registerServiceWorker: service worker registration failed > ${error.printStackTrace()}",
                )
            }
      }

  object LaminarRoundTripCalculator {
    import com.raquo.laminar.api.L._

    def app(
      containerId: String,
    ) = {
      import org.scalajs.dom
      val app = div(
        LaminarRoundTripCalculator.RoundTripCalculator(),
      )
      render(dom.document.getElementById(containerId), app)
    }

    val actionVar = Var("Do the thing")

    val routes =
      List(RtaNorthbound.fullSchedule, RtaSouthbound.fullSchedule)

    val startingLocationChoices =
      routes
        .map(
          namedRoute =>
            (namedRoute.routeName.name,
             namedRoute.routeWithTimes.allInvolvedStops),
        )
        .toMap

    val startingRouteSelections = new EventBus[String]

    val $startRouteVar: Var[NamedRoute] = Var(routes.head)

    val rawNamesToTypes: EventStream[NamedRoute] =
      startingRouteSelections.events.map {
        case newVal =>
          routes
            .find(_.routeName.name == newVal)
            .getOrElse(
              throw new RuntimeException(
                "Unexpected RouteName " + newVal,
              ),
            )
      }

    val $route: Var[String] =
      Var(
        initial = routes.head.routeName.name,
      )

    case class SelectValue(
      uniqueValue: String,
      humanFriendlyName: String)

    def Selector(
      route: Seq[SelectValue],
      eventStream: WriteBus[SelectValue],
    ) = {
      val $curValue = Var(route.head)
      div(
        select(
          inContext {
            thisNode =>
              onChange
                .mapTo(thisNode.ref.value)
                .map(
                  uniqueValue =>
                    route.find(_.uniqueValue == uniqueValue).get,
                ) --> eventStream
          },
          value <-- $curValue.signal.map(_.uniqueValue),
          route.map(
            stop =>
              option(value(stop.uniqueValue), stop.humanFriendlyName),
          ),
        ),
      )
    }

    def RoundTripCalculator() = {

      val startingPoint = new EventBus[SelectValue]
      div(
        select(
          inContext {
            thisNode =>
              onChange
                .mapTo(thisNode.ref.value) --> startingRouteSelections
          },
          rawNamesToTypes --> $startRouteVar.writer,
          value <-- $startRouteVar.signal
            .map(_.routeName.name),
          routes.map(
            route =>
              option(value(route.routeName.name),
                     route.routeName.userFriendlyName),
          ),
        ),
        child <-- $startRouteVar.signal.map(
          namedRoute =>
            Selector(
              namedRoute.routeWithTimes.legs.head.stops.map(
                locationWithTime =>
                  SelectValue(locationWithTime.location.name,
                              locationWithTime.location.name),
              ),
              startingPoint.writer,
            ),
        ),
      )
    }

  }
}
