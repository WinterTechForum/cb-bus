package crestedbutte.laminar

import com.billding.time.{TimePicker, WallTime}
import crestedbutte.*
import crestedbutte.TripBoundary.ArrivingBy
import crestedbutte.pwa.Persistence
import crestedbutte.routes.{RtaNorthbound, RtaSouthbound}
import org.scalajs.dom
import org.scalajs.dom.{
  window,
  IDBDatabase,
  IDBEvent,
  IDBOpenDBRequest,
  IDBTransactionMode,
  MouseEvent,
}
import website.webcomponents.material.{Button, SmartTimePicker}

object LaminarTripPlanner {
  import com.raquo.laminar.api.L._

  val componentName = ComponentName("TripPlanner")

  def app() = {
    import org.scalajs.dom
    val db: Var[Option[IDBDatabase]] = Var(
      None,
    ) // TODO Give a real DB value to restore functionality
    val app = div(
      LaminarTripPlanner.TripPlannerLaminar(WallTime("2:00 PM"), db),
    )
    render(
      dom.document.getElementById(componentName.name),
      app,
    )
  }

  def TripPlannerLaminar(
    initialTime: WallTime,
    db: Var[Option[IDBDatabase]],
  ) = {
    val routes =
      List(RtaNorthbound.fullSchedule, RtaSouthbound.fullSchedule)

    val $currentRoute: Var[NamedRoute] = Var(
      RtaSouthbound.fullSchedule,
    )
    val $tripBoundary: Var[TripBoundary] = Var(ArrivingBy)

    val $startingPoint: Var[Location] = Var(
      $currentRoute.now().firstStopOnRoute,
    )

    println("Last stop: " + $currentRoute.now().lastStopOnRoute)

    val $destination: Var[Location] = Var(
      $currentRoute.now().lastStopOnRoute,
    )

    val TimePicker(timePicker, arrivalTimeS) =
      TimePicker(initialTime.toDumbAmericanString)

    val changeBus = new EventBus[Unit]

    val clickBus = new EventBus[Unit]

    val submissionZ = new EventBus[TripParamZ]

    val tripResult: EventStream[Either[TripPlannerError, RouteLeg]] =
      submissionZ.events
        .map(_.evaluate())
    val $plan = Var(Plan(Seq.empty))

    val valuesDuringChangeZ: EventStream[TripParamZ] =
      changeBus.events
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
      onMountCallback: context =>
        println("Mounted trip planner"),
      // TODO Cleaner way of updating Trip based on changes to any of these params
      button(
        cls := "button",
        "Get saved plan",
        onClick --> Persistence.retrieveDailyPlan($plan, db),
      ),
      button(
        cls := "button",
        "Delete saved plan",
        onClick --> Persistence.saveDailyPlan(
          crestedbutte.Plan(Seq.empty),
          db,
        ),
      ),
      valuesDuringChangeZ --> submissionZ,
      $startingPoint.signal
        .combineWith($destination,
                     $currentRoute,
                     $tripBoundary,
                     arrivalTimeS,
        )
        .changes
        .map(_ => ()) --> changeBus.writer,
      Components.RouteSelector($currentRoute,
                               $startingPoint,
                               $destination,
      ),
      Components.StopSelector("Starting from: ",
                              $startingPoint,
                              $currentRoute,
      ),
      // TODO Fix: this should have a later stop selected by default
      Components.StopSelector("Reaching: ",
                              $destination,
                              $currentRoute,
      ),
      Components.TripBoundarySelector($tripBoundary),
      timePicker,
      div(
        child <-- tripResult.map:
          case Left(value) =>
            div:
              "Trip not possible."
          case Right(value) =>
            TagsOnlyLocal.RouteLegEnds(value, $plan),
      ),
      child <-- $plan.signal.map(TagsOnlyLocal.Plan(_, db)),
    )
  }

}
