package crestedbutte.laminar

import com.billding.time.{TimePicker, WallTime}
import crestedbutte.*
import crestedbutte.TripBoundary.ArrivingBy
import crestedbutte.routes.{RtaNorthbound, RtaSouthbound}
import org.scalajs.dom
import org.scalajs.dom.MouseEvent
import website.webcomponents.material.{Button, SmartTimePicker}

object LaminarTripPlanner {
  import com.raquo.laminar.api.L._

  val componentName = RouteName("TripPlanner")

  def app() = {
    import org.scalajs.dom
    val app = div(
      LaminarTripPlanner.TripPlannerLaminar(WallTime("2:00 PM")),
    )
    render(
      dom.document.getElementById(componentName.name),
      app,
    )
  }

  def TripPlannerLaminar(
    initialTime: WallTime,
  ) = {
    val routes =
      List(RtaNorthbound.fullSchedule, RtaSouthbound.fullSchedule)

    val $currentRoute: Var[NamedRoute] = Var(
      RtaSouthbound.fullSchedule,
    )
    val $tripBoundary: Var[TripBoundary] = Var(ArrivingBy)
    val startingRouteSelections = new EventBus[String]

    val returnRoute: Signal[NamedRoute] =
      $currentRoute.signal.map(startRoute =>
        routes.find(_ != startRoute).get,
      )

    val $returnRouteVar = Var(routes.last)

    val $startingPoint: Var[Location] = Var(
      $currentRoute.now().firstStopOnRoute,
    )

    val $destination: Var[Location] = Var(
      $currentRoute.now().lastStopOnRoute
    )

    val TimePicker(timePicker, arrivalTimeS) =
      TimePicker(initialTime.toDumbAmericanString)

    val clickBus = new EventBus[Unit]
    val valuesDuringClick: EventStream[WallTime] =
      clickBus.events.withCurrentValueOf(arrivalTimeS)

    val submissionBehavior =
      Observer[WallTime](
        onNext = busTime => println("time @ click: " + busTime),
      )

    val TimePicker(departureTimePicker, departureTimeS) =
      TimePicker(initialTime = "5:00 PM")

    val submissionZ = new EventBus[TripParamZ]

    val tripResult: EventStream[Either[TripPlannerError, RouteLeg]] =
      submissionZ.events
        .map(_.evaluate())

    val valuesDuringRealSubmissionZ: EventStream[TripParamZ] =
      clickBus.events
        .withCurrentValueOf(
          $startingPoint,
          $destination,
          arrivalTimeS,
          $currentRoute,
          $tripBoundary,
        )
        .map:
          case (
                startingPoint,
                destination,
                arrivalTime,
                startRoute,
                tripBoundary,
              ) =>
            tripBoundary match
              case TripBoundary.StartingAfter =>
                TripParamZ.StartingAfter(
                  startingPoint,
                  arrivalTime,
                  startRoute.routeWithTimes,
                  destination,
                )
              case TripBoundary.ArrivingBy =>
                TripParamZ.ArrivingBy(
                  startingPoint,
                  arrivalTime,
                  destination,
                  startRoute.routeWithTimes,
                )

    div(
      Components.RouteSelector($currentRoute),
      Components.StopSelector("Starting from: ",  $startingPoint.writer, $currentRoute),
      Components.StopSelector("Reaching: ",  $destination.writer, $currentRoute),
      Components.TripBoundarySelector($tripBoundary),
      timePicker,
      div(
        button(
          "Plan Trip",
          onClick.map(_ => ()) --> clickBus,
          valuesDuringRealSubmissionZ --> submissionZ,
        ),
      ),
      div(
        child <-- tripResult.map: res =>
          div:
            res match
              case Left(value)  => "Trip not possible."
              case Right(value) => TagsOnlyLocal.RouteLegEnds(value),
      ),
      valuesDuringClick --> submissionBehavior,
      returnRoute --> $returnRouteVar.writer,
//      div(
//        child <-- roundTripResults.map {
//          case Left(tripPlannerError: TripPlannerError) =>
//            div(
//              "Could not plan a route: " + tripPlannerError.msg,
//            )
//          case Right(roundTripResult) =>
//            renderRoundTrip(roundTripResult)
//        },
//      ),
    )
  }

  def renderLeg(
    routeLeg: RouteLeg,
  ) =
    routeLeg.stops match {
      case (head :: rest) :+ last =>
        span(
          head.location.name + " @ " + head.busTime.toDumbAmericanString + " => " + last.location.name + " @ " + last.busTime.toDumbAmericanString,
        )
      case unhandled =>
        throw new RuntimeException("shit: " + unhandled)
    }

  def renderRoundTrip(
    roundTrip: Trip,
  ) =
    div(
      h2(cls := "title is-2", "Your trip:"),
      h3(cls := "title is-3", "Outward"),
      div(renderLeg(roundTrip.leave)),
      h3(cls := "title is-3", "Homeward"),
      div(renderLeg(roundTrip.returnLeg)),
    )

  def renderTrip(
    routeLeg: RouteLeg,
  ) =
    div(
      h2(cls := "title is-2", "Your trip:"),
      div(renderLeg(routeLeg)),
    )

}
