package crestedbutte.routes

import crestedbutte.RouteSegment

object RouteLookup {
  def apply(routeSegment: RouteSegment): RouteWithTimes =
    routeSegment.route match
      case RtaSouthbound.componentName =>
        RtaSouthbound.fullSchedule.routeWithTimes
      case RtaNorthbound.componentName =>
        RtaNorthbound.fullSchedule.routeWithTimes
      case other =>
        throw new Exception("Unrecognized route: " + other)
}
