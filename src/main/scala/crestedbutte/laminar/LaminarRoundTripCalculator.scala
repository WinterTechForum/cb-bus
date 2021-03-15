package crestedbutte.laminar

import com.billding.time.{BusDuration, BusTime}
import crestedbutte.{
  Location,
  LocationWithTime,
  RoundTrip,
  RoundTripCalculator,
  RoundTripParams,
  RouteLeg,
  RouteName,
  TripPlannerError,
}
import crestedbutte.routes.{RtaNorthbound, RtaSouthbound}
import org.scalajs.dom
import org.scalajs.dom.MouseEvent
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

    val defaultArrivalTime = BusTime("07:10")
    val (arrivalTimeS, arrivalTimePicker) =
      TimePicker.TimePicker(defaultArrivalTime)

    val clickBus = new EventBus[Unit]
    val valuesDuringClick: EventStream[BusTime] =
      clickBus.events.withCurrentValueOf(arrivalTimeS)

    val submissionBehavior =
      Observer[BusTime](
        onNext = (busTime) => println("time @ click: " + busTime),
      )

    val defaultDepartureTime = defaultArrivalTime.plusMinutes(60)
    val (departureTimeS, departureTimePicker) =
      TimePicker.TimePicker(defaultDepartureTime)

    val submissions = new EventBus[RoundTripParams]
    val roundTripResults
      : EventStream[Either[TripPlannerError, RoundTrip]] =
      submissions.events
        .map(RoundTripCalculator.calculate)

    val startingPointOptions =
      $startRouteVar.signal
        .map(_.allStops)
        .map(
          Selector(
            _,
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

    val realSubmissionBehavior =
      Observer[
        (Location.Value,
         Location.Value,
         BusTime,
         NamedRoute,
         BusTime,
         Location.Value,
         NamedRoute),
      ](onNext = {
        case (startingPoint,
              destination,
              arrivalTime,
              startRoute,
              departureTime,
              returnStartPoint,
              returnRoute) =>
          RoundTripParams(
            startingPoint,
            destination,
            arrivalTime,
            startRoute.routeWithTimes,
            departureTime,
            returnStartPoint,
            returnRoute.routeWithTimes,
          )
      })

    val valuesDuringRealSubmission: EventStream[RoundTripParams] =
      clickBus.events
        .withCurrentValueOf($startingPoint,
                            $destination,
                            arrivalTimeS,
                            $startRouteVar,
                            departureTimeS,
                            $returnStartPoint,
                            $returnRouteVar)
        .map {
          case (startingPoint,
                destination,
                arrivalTime,
                startRoute,
                departureTime,
                returnStartPoint,
                returnRoute) =>
            RoundTripParams(
              startingPoint,
              destination,
              arrivalTime,
              startRoute.routeWithTimes,
              departureTime,
              returnStartPoint,
              returnRoute.routeWithTimes,
            )
        }

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
        child <-- destinationOptions,
      ),
      div("At: ", arrivalTimePicker),
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
      div("after: ", departureTimePicker),
      div(
        button(
          "Plan Trip",
          onClick.map(_ => ()) --> clickBus,
          valuesDuringRealSubmission --> submissions,
        ),
      ),
      valuesDuringClick --> submissionBehavior,
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
