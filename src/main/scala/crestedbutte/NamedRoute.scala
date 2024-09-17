package crestedbutte

import com.billding.time.WallTime
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

  def segment(start: Location, end: Location): Option[Seq[RouteSegment]] =
    val result =
      routeWithTimes.legs
        .flatMap { leg =>
          leg.segmentFrom(start, end)
        }
        .filter(segment => 
          WallTime.ordering.compare(segment.start.t, segment.end.t) <= 0
        )
    Option.when{
//      import math. Ordered. orderingToOrdered
//      val res =
//        // TODO Use 0,-1,1
//        result.find(segment => segment.start.t.compare(segment.end.t))
//      println("res: " + res)
      result.nonEmpty
    }{
      result
      }
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
