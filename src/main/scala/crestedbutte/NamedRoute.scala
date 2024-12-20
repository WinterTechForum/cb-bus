package crestedbutte

import com.billding.time.WallTime
import crestedbutte.routes.RouteWithTimes
import crestedbutte.{Location, RouteName}

// TODO Where should this ComponentData abstraction *actually* live?
sealed trait ComponentData {
  //  namedRoute: NamedRoute,

  val componentName: RouteName
}

case object PlanViewer extends ComponentData {
  val componentName = RouteName("PlanViewer")
}

import crestedbutte.{Location, RouteName}

case class NamedRoute(
  componentName: RouteName,
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

  def segment(
    start: Location,
    end: Location,
  ): Option[Seq[RouteSegment]] =
    val result =
      routeWithTimes.legs
        .flatMap { leg =>
          leg.segmentFrom(start, end)
        }
        .filter(segment =>
          WallTime.ordering
            .compare(segment.start.t, segment.end.t) <= 0,
        )
    Option.when {
      result.nonEmpty
    } {
      result
    }
}

object NamedRoute {

  def apply(
    rawRouteName: String,
    routeWithTimes: RouteWithTimes,
  ): NamedRoute =
    NamedRoute(
      RouteName(rawRouteName),
      routeWithTimes,
    )
}
