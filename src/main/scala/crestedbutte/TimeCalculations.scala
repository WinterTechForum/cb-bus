package crestedbutte

import crestedbutte.laminar.LocationTimeDirection
import crestedbutte.routes.{RtaNorthbound, RtaSouthbound}

object TimeCalculations {
  def updateSegmentFromArbitrarySelection(
    ltd: LocationTimeDirection,
    plan: Plan,
  ) = {
    val other =
      if (ltd.routeSegment.start.l == ltd.locationWithTime.l)
        ltd.routeSegment.end.l
      else if (ltd.routeSegment.end.l == ltd.locationWithTime.l)
        ltd.routeSegment.start.l
      else
        throw new RuntimeException("WTF")
    println("ltd.routeSegment.route: " + ltd.routeSegment.route)
    val routeWithTimes =
      ltd.routeSegment.route match
        case RtaSouthbound.componentName =>
          RtaSouthbound.normalRouteWithTimes
        case RtaNorthbound.componentName =>
          RtaNorthbound.normalRouteWithTimes
        case other =>
          throw new IllegalArgumentException(
            "Route not supported: " + other,
          )
    val newRouteLegThatShouldBeUsedForUpdatingOtherStop =
      routeWithTimes.legs
        .find(routeLeg =>
          routeLeg.stops.contains(ltd.locationWithTime),
        )
        .getOrElse(throw new IllegalStateException("boof"))
    val newOtherValue =
      newRouteLegThatShouldBeUsedForUpdatingOtherStop.stops
        .find(lwt => lwt.l == other)
        .getOrElse(throw new IllegalStateException("doof"))
    val res =
      plan.copy(l = plan.l.map { routeSegment =>
        routeSegment
          .updateTimeAtLocation(
            ltd.locationWithTime,
            ltd.routeSegment.start.t,
            ltd.routeSegment.end.t,
          )
          .updateTimeAtLocation(
            newOtherValue,
            ltd.routeSegment.start.t,
            ltd.routeSegment.end.t,
          )
      })
    res
  }

}
