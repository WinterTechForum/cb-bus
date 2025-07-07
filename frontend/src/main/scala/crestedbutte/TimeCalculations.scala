package crestedbutte

import com.billding.time.WallTime
import crestedbutte.routes.RTA.{Northbound, Southbound}
import crestedbutte.routes.RouteWithTimes
import crestedbutte.laminar.LocationTimeDirection
import scala.util.{Either, Left, Right}

object TimeCalculations {
  def updateSegmentFromArbitrarySelection(
    ltd: LocationTimeDirection,
    plan: Plan,
  ) =
    for {
      other <-
        if (ltd.routeSegment.start.l == ltd.locationWithTime.l)
          Right(ltd.routeSegment.end.l)
        else if (ltd.routeSegment.end.l == ltd.locationWithTime.l)
          Right(ltd.routeSegment.start.l)
        else
          Left("WTF Invalid: " + ltd)
    } yield {

      println("ltd.routeSegment.route: " + ltd.routeSegment.route)
      val routeWithTimes =
        ltd.routeSegment.route match
          case Southbound.componentName =>
            Southbound.normalRouteWithTimes
          case Northbound.componentName =>
            Northbound.normalRouteWithTimes
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

  def getRouteWithTimes(
    routeName: RouteName,
  ): RouteWithTimes =
    routeName match {
      case Southbound.componentName =>
        Southbound.normalRouteWithTimes
      case Northbound.componentName =>
        Northbound.normalRouteWithTimes
      case _ =>
        throw new IllegalArgumentException(
          s"Unknown route: $routeName",
        )
    }
}
