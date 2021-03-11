package crestedbutte.laminar

import crestedbutte.routes.RouteWithTimes
import crestedbutte.{Location, RouteName}

sealed trait ComponentData {
//  namedRoute: NamedRoute,

  val componentName: RouteName
}

case object RoundTripCalculatorComponent extends ComponentData {

  val componentName =
    LaminarRoundTripCalculator.calculatorComponentName
}

case class NamedRoute(
  routeName: RouteName,
  routeWithTimes: RouteWithTimes)
    extends ComponentData {
  val componentName = routeName // TODO rm this and depend on routeName. Too much indirection going on!

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

  val allStops: Seq[Location.Value] =
    routeWithTimes.legs.head.stops.map(_.location)
}

object NamedRoute {

  def apply(
    rawRouteName: String,
    routeWithTimes: RouteWithTimes,
  ): Unit =
    NamedRoute(
      RouteName(rawRouteName),
      routeWithTimes,
    )
}
