package crestedbutte

import crestedbutte.routes.RouteWithTimes

case class NamedRoute(
  routeName: RouteName,
  routeWithTimes: RouteWithTimes) {

  def stopsRemainingAfter(
    startPoint: Location.Value,
  ): Seq[Location.Value] =
    routeWithTimes.allInvolvedStops.drop(
      routeWithTimes.allInvolvedStops
        .indexWhere(
          involvedStop => involvedStop.name == startPoint.name,
        ) + 1, // Only include stops beyond the current stop
    )

  val firstStopOnRoute: Location.Value =
    routeWithTimes
      .routeLeg(0)
      .stops
      .head // todo unsafe
      .location

  val lastStopOnRoute: Location.Value =
    routeWithTimes
      .routeLeg(0)
      .stops
      .last // todo unsafe
      .location
}

object NamedRoute {

  def apply(
    rawRouteName: String,
    routeWithTimes: RouteWithTimes,
  ): Unit =
    NamedRoute(
      new RouteName(rawRouteName),
      routeWithTimes,
    )
}
