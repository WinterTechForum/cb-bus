package crestedbutte.routes

import crestedbutte.{Location, RouteName}
import crestedbutte.routes.RouteWithTimes
import crestedbutte.routes.RTA.{Northbound, Southbound}

object RouteLookup {
  def lookupRouteWithTimes(
    routeName: RouteName,
  ): RouteWithTimes =
    routeName match {
      case Southbound.componentName =>
        Southbound.fullSchedule.routeWithTimes
      case Northbound.componentName =>
        Northbound.fullSchedule.routeWithTimes
      case _ =>
        throw new IllegalArgumentException(
          s"Unknown route: $routeName",
        )
    }
}
