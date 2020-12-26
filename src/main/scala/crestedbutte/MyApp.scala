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
import org.scalajs.dom.document
import org.scalajs.dom.raw.HTMLElement

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
      _ <- BulmaBehaviorLocal.addMenuBehavior(
        loopingLogic,
      )
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
                .render,
              "upcoming-buses",
            )
          } yield ()
        case ComponentDataTyped(value, componentName) => {
          println("huh?")
          for {
            _ <- ZIO {
              show(componentName.name)
            }
          } yield ()
        }
      }
    }
    else {
      println("hiding: " + componentData.componentName.name)
      DomManipulation.hideElement(
        componentData.componentName.name,
      )
    }

  // a decent library would also have this function
  private def show(
    elementId: String,
  ) = {
    println("!trying to show " + s"#$elementId")

    val result =
      document.body
        .querySelector(s"#$elementId")
    println(result.getAttribute("class"))
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

  object LaminarRoundTripCalculator {
    import com.raquo.laminar.api.L._

    val calculatorComponentName = RouteName("RoundTripCalculator")

    def app() = {
      import org.scalajs.dom
      val app = div(
        LaminarRoundTripCalculator.RoundTripCalculatorLaminar(),
      )
      render(
        dom.document.getElementById(calculatorComponentName.name),
        app,
      )
    }

    case class SelectValue(
      uniqueValue: String,
      humanFriendlyName: String)

    def Selector[T](
      route: Seq[T],
      eventStream: WriteBus[T],
      converterThatCouldBeATypeClass: T => SelectValue,
    ) = {
      val valueMap: Map[SelectValue, T] =
        route
          .map(
            selectValue =>
              (converterThatCouldBeATypeClass(selectValue),
               selectValue),
          )
          .toMap
      val selectValues = route.map(converterThatCouldBeATypeClass)
      span(
        cls := "select is-rounded",
        select(
          inContext {
            thisNode =>
              onChange
                .mapTo(thisNode.ref.value)
                .map(
                  uniqueValue =>
                    selectValues
                      .find(_.uniqueValue == uniqueValue)
                      .get,
                )
                .map(
                  valueMap.getOrElse(_,
                                     throw new RuntimeException(
                                       "can't find the value!",
                                     )),
                ) --> eventStream
          },
          selectValues.map(
            stop =>
              option(value(stop.uniqueValue), stop.humanFriendlyName),
          ),
        ),
      )
    }

    sealed trait DayTime
    case object AM extends DayTime
    case object PM extends DayTime

    def TimePicker(
      timeStream: Observer[Option[BusTime]],
      valueName: String,
    ) = {
      val timeString = Var("08:00")
      val daytime: Var[DayTime] = Var(AM)

      val typedTime
        : Signal[Option[BusTime]] = // new EventBus[Option[BusTime]]
        timeString.signal.map(rawTime => {
          try {
            Some(BusTime(rawTime))
          } catch {
            case ex: Exception => None
          }
        })

      span(
        input(`type` := "text",
              defaultValue := timeString.now(),
              inContext {
                thisNode =>
                  onInput.map(_ => thisNode.ref.value) --> timeString
              },
              name := (valueName + "_time")),
        input(`type` := "radio",
              value := AM.toString,
              name := (valueName + "_AM-OR-PM"),
              idAttr := "AM",
              checked := true,
              onClick.map {
                _ =>
                  AM
              } --> daytime,
              AM.toString),
        label(forId := "AM", "AM"),
        input(`type` := "radio",
              value := PM.toString,
              name := (valueName + "_AM-OR-PM"),
              idAttr := "PM",
              onClick.map {
                _ =>
                  PM
              } --> daytime,
              PM.toString),
        label(forId := "PM", "PM"),
        typedTime.signal.combineWith(daytime.signal).map {
          case (maybeTime, dayTime) =>
            maybeTime match {
              case Some(busTime) =>
                if (dayTime == AM) Some(busTime)
                else Some(busTime.plus(BusDuration.ofMinutes(720)))
              case None => None
            }
        } --> timeStream,
      )
    }

    def RoundTripCalculatorLaminar() = {
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

      val returnRoute: Signal[NamedRoute] =
        $startRouteVar.signal.map(
          startRoute => routes.find(_ != startRoute).get,
        )

      val $returnRouteVar = Var(routes.last)

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

      def locationWithTime2selectorValue(
        locationWithTime: LocationWithTime,
      ): SelectValue =
        location2selectorValue(locationWithTime.location)

      def location2selectorValue(
        location: Location.Value,
      ): SelectValue =
        SelectValue(location.name, location.name)

      val startingPoint = new EventBus[LocationWithTime]
      val $startingPoint: Var[Option[LocationWithTime]] = Var(None)
      val destination = new EventBus[Location.Value]
      val $destination: Var[Option[Location.Value]] = Var(None)
      val returnStartPoint = new EventBus[LocationWithTime]
      val $returnStartPoint: Var[Option[LocationWithTime]] = Var(None)
      val arrivalTime: Var[Option[BusTime]] = Var(None)
      val departureTime: Var[Option[BusTime]] = Var(None)
      val submissions = new EventBus[RoundTripParams]
      val roundTripResults: EventStream[RoundTrip] =
        submissions.events
          .map(
            roundTripParams =>
              RoundTripCalculator.calculate(roundTripParams),
          )

      val submissionActions: EventStream[Unit] =
        submissions.events.map {
          submission =>
            println("arrivalTime: " + arrivalTime.now())
            println("departureTime: " + departureTime.now())
        }

//      import website.webcomponents.material.Button
      import typings.materialUiPickers.pickerMod.Picker
      val theVoid = new EventBus[Unit]
      div(
        div(
          "On this line:",
          span(
            cls := "select is-rounded",
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
          ),
        ),
        div(
          "Starting from: ",
          child <-- $startRouteVar.signal
            .map(_.routeWithTimes.legs.head.stops)
            .map(
              stops =>
                Selector(
                  stops,
                  startingPoint.writer,
                  locationWithTime2selectorValue,
                ),
            ),
          startingPoint.events.map(Some(_)) --> $startingPoint.writer,
        ),
        div(
          "and reaching: ",
          child <--
          startingPoint.events
            .map(
              startPoint => {
                val startRouteNow = $startRouteVar.now()

                val remainingStops: Seq[Location.Value] =
                  startRouteNow.routeWithTimes.allInvolvedStops.drop(
                    startRouteNow.routeWithTimes.allInvolvedStops
                      .indexWhere(
                        involvedStop =>
                          involvedStop.name == startPoint.location.name,
                      ) + 1, // Only include stops beyond the current stop
                  )

                Selector(
                  remainingStops,
                  destination.writer,
                  location2selectorValue,
                )
              },
            ),
          destination.events.map(Some(_)) --> $destination.writer,
        ),
        div("At: ", TimePicker(arrivalTime.writer, "arrivalTime")),
        div(
          "And returning from: ",
          child <-- returnRoute
            .map(_.routeWithTimes.legs.head.stops)
            .map(
              stops =>
                Selector(
                  stops,
                  returnStartPoint.writer,
                  locationWithTime2selectorValue,
                ),
            ),
          returnStartPoint.events
            .map(Some(_)) --> $returnStartPoint.writer,
        ),
        div("after: ",
            TimePicker(departureTime.writer, "departureTime"),
        ),
        div(
          button(
            "Plan Trip",
            onClick.map {
              _ =>
                println("Clicked!")
                println("startingPoint: " + $startingPoint.now())
                println("arrivalTime: " + arrivalTime.now())
                println("destination: " + $destination.now())
                println("departureTime: " + departureTime.now())
                println(
                  "returnStartPoint: " + $returnStartPoint.now(),
                )
//                "click!"

                RoundTripParams(
                  $startingPoint.now().get.location, // startLocation: Location.Value,
                  $destination.now().get, // destination: Location.Value,
                  arrivalTime.now().get,
                  $startRouteVar.now().routeWithTimes,
                  arrivalTime
                    .now()
                    .get
                    .between(departureTime.now().get),
                  $returnStartPoint.now().get.location, // returningLaunchPoint: Location.Value,
                  $returnRouteVar.now().routeWithTimes,
                )

            } --> submissions,
          ),
        ),
        submissionActions --> theVoid,
        returnRoute --> $returnRouteVar.writer,
        div(
          child <-- roundTripResults.map(
            (roundTripResult: RoundTrip) =>
              renderRoundTrip(roundTripResult),
          ),
        ),
      )
    }

    def renderLeg(
      routeLeg: RouteLeg,
    ) =
      routeLeg.stops match {
        case (head :: rest) :+ last =>
          span(
            head.location + " @ " + head.busTime.toDumbAmericanString + " => " + last.location + " @ " + last.busTime.toDumbAmericanString,
          )
        case unhandled =>
          throw new RuntimeException("shit: " + unhandled)
      }

    def renderRoundTrip(
      roundTrip: RoundTrip,
    ) =
      div(
        h2(cls := "title is-2", "Your trip:"),
        h3(cls := "title is-3", "Outward"),
        div(renderLeg(roundTrip.leave)),
        h3(cls := "title is-3", "Homeward"),
        div(renderLeg(roundTrip.returnLeg)),
      )

  }
}
