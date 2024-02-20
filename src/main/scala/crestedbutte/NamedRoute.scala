package crestedbutte

import crestedbutte.laminar.LaminarTripPlanner
import crestedbutte.routes.RouteWithTimes
import crestedbutte.{ComponentName, Location}

// TODO Where should this ComponentData abstraction *actually* live?
sealed trait ComponentData {
  //  namedRoute: NamedRoute,

  val componentName: ComponentName
}

case object PlanViewer extends ComponentData {
  val componentName = ComponentName("PlanViewer")
}

case object TripPlannerComponent extends ComponentData {

  val componentName =
    LaminarTripPlanner.componentName
}

import crestedbutte.{ComponentName, Location}

case class NamedRoute(
  componentName: ComponentName,
  routeWithTimes: RouteWithTimes)
    extends ComponentData {

  def stopsRemainingAfter(
    startPoint: Location,
  ): Seq[Location] =
    routeWithTimes.allInvolvedStops.drop(
      routeWithTimes.allInvolvedStops
        .indexWhere(involvedStop =>
          involvedStop.name == startPoint.name,
        ) + 1, // Only include stops beyond the current stop
    )

  val firstStopOnRoute: Location =
    routeWithTimes
      .firstRouteLeg()
      .head
      .location

  val lastStopOnRoute: Location =
    routeWithTimes
      .firstRouteLeg()
      .last
      .location

  val allStops: Seq[Location] =
    routeWithTimes.legs.head.stops.map(_.location)
}

object NamedRoute {

  def apply(
    rawRouteName: String,
    routeWithTimes: RouteWithTimes,
  ): NamedRoute =
    NamedRoute(
      ComponentName(rawRouteName),
      routeWithTimes,
    )
}
