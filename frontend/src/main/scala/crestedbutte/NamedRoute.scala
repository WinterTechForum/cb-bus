package crestedbutte

import com.billding.time.WallTime
import crestedbutte.routes.RouteWithTimes

case class NamedRoute(
  componentName: RouteName,
  routeWithTimes: RouteWithTimes) {

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
