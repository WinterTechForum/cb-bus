package crestedbutte

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

import crestedbutte.{ComponentName, Location}

case class NamedRoute(
  componentName: ComponentName,
  routeWithTimes: RouteWithTimes)
    extends ComponentData {

  val firstStopOnRoute: Location =
    routeWithTimes
      .firstRouteLeg()
      .head
      .l

  val lastStopOnRoute: Location =
    routeWithTimes
      .firstRouteLeg()
      .last
      .l

  val allStops: Seq[Location] =
    routeWithTimes.legs.head.stops.map(_.l)
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
