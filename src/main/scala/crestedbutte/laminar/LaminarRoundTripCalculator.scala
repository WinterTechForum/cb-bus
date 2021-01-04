package crestedbutte.laminar

import com.billding.time.{BusDuration, BusTime}
import crestedbutte.{
  Location,
  LocationWithTime,
  NamedRoute,
  RoundTrip,
  RoundTripCalculator,
  RoundTripParams,
  RouteLeg,
  RouteName,
  TripPlannerError,
}
import crestedbutte.routes.{RtaNorthbound, RtaSouthbound}
import website.webcomponents.material.{Button, SmartTimePicker}

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
    eventStream: Observer[T],
  )(implicit converterThatCouldBeATypeClass: T => SelectValue,
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

  def TimePickerLocalSuccessOnly(
    timeStream: Observer[BusTime],
    initialValue: BusTime,
    valueName: String,
  ) = {
    val timeString = Var(initialValue.toString)
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
      typedTime.signal
        .combineWith(daytime.signal)
        .map {
          case (maybeTime, dayTime) =>
            maybeTime match {
              case Some(busTime) =>
                if (dayTime == AM) Some(busTime)
                else Some(busTime.plus(BusDuration.ofMinutes(720)))
              case None => None
            }
        }
        .changes
        .filter(_.isDefined)
        .map(_.get) --> timeStream,
    )
  }

  def RoundTripCalculatorLaminar() = {
    val routes =
      List(RtaNorthbound.fullSchedule, RtaSouthbound.fullSchedule)

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

    implicit val location2selectorValue
      : Location.Value => SelectValue = (location: Location.Value) =>
      SelectValue(location.name, location.name)

    val $startingPoint: Var[Location.Value] = Var(
      $startRouteVar.now().firstStopOnRoute,
    )
    val initialDestination =
      $startRouteVar.now().lastStopOnRoute

    val $destination: Var[Location.Value] = Var(
      initialDestination,
    )
    val $returnStartPoint: Var[Location.Value] = Var(
      initialDestination,
    )
    val arrivalTime: Var[BusTime] = Var(BusTime("07:00"))
    val departureTime: Var[BusTime] = Var(
      arrivalTime.now().plusMinutes(60),
    )
    val submissions = new EventBus[RoundTripParams]
    val roundTripResults
      : EventStream[Either[TripPlannerError, RoundTrip]] =
      submissions.events
        .map(
          roundTripParams =>
            RoundTripCalculator.calculate(roundTripParams),
        )

    val startingPointOptions =
      $startRouteVar.signal
        .map(_.routeWithTimes.legs.head.stops.map(_.location))
        .map(
          stops =>
            Selector(
              stops,
              $startingPoint.writer,
            ),
        )

    val destinationOptions =
      $startingPoint.signal
        .map(
          (startPoint: Location.Value) => {
            Selector(
              $startRouteVar.now().stopsRemainingAfter(startPoint),
              $destination.writer,
            )
          },
        )

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
        child <-- startingPointOptions,
      ),
      div(
        "and reaching: ",
        child <--
        destinationOptions,
      ),
      div(
        "At: ",
        TimePickerLocalSuccessOnly(arrivalTime.writer,
                                   arrivalTime.now(),
                                   "arrivalTime"),
        div("bap"),
      ),
      //            materialsButton()),
      div(
        "And returning from: ",
        child <-- returnRoute
          .map(_.routeWithTimes.legs.head.stops.map(_.location)) // todo turn into a function
          .map(
            stops =>
              Selector(
                stops,
                $returnStartPoint.writer,
              ),
          ),
      ),
      div("after: ",
          TimePickerLocalSuccessOnly(departureTime.writer,
                                     departureTime.now(),
                                     "departureTime"),
      ),
      div(
        button(
          "Plan Trip",
          onClick.map(
            _ =>
              RoundTripParams(
                $startingPoint.now(),
                $destination.now(),
                arrivalTime.now(),
                $startRouteVar.now().routeWithTimes,
                arrivalTime
                  .now()
                  .between(departureTime.now()),
                $returnStartPoint.now(),
                $returnRouteVar.now().routeWithTimes,
              ),
          ) --> submissions,
        ),
      ),
      returnRoute --> $returnRouteVar.writer,
      div(
        child <-- roundTripResults.map {
          case Left(tripPlannerError: TripPlannerError) =>
            div(
              "Could not plan a route: " + tripPlannerError.msg,
            )
          case Right(roundTripResult) =>
            renderRoundTrip(roundTripResult)
        },
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
