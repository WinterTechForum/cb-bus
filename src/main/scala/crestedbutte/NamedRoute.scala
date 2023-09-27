package crestedbutte

import crestedbutte.laminar.LaminarTripPlanner
import crestedbutte.routes.RouteWithTimes
import crestedbutte.{Location, ComponentName}

// TODO Where should this ComponentData abstraction *actually* live?
sealed trait ComponentData {
  //  namedRoute: NamedRoute,

  val componentName: ComponentName
}

case object TripPlannerComponent extends ComponentData {

  val componentName =
    LaminarTripPlanner.componentName
}

import crestedbutte.{Location, ComponentName}

case class NamedRoute(
                       routeName: ComponentName,
                       routeWithTimes: RouteWithTimes)
    extends ComponentData {
  val componentName =
    routeName // TODO rm this and depend on routeName. Too much indirection going on!

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
      .routeLeg(0)
      .stops
      .head // todo unsafe
      .location

  val lastStopOnRoute: Location =
    routeWithTimes
      .routeLeg(0)
      .stops
      .last // todo unsafe
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
