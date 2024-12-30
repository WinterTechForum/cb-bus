package crestedbutte.routes

import crestedbutte.{
  BusSchedule,
  BusScheduleAtStop,
  Location,
  LocationWithTime,
  RouteLeg,
  RouteName,
  RouteSegment,
}
import com.billding.time.WallTime

case class RouteWithTimes(
  // TODO Should be a map representing all the connections.
  legs: Seq[RouteLeg]) {

  private def indexOfLegThatContains(
    other: RouteSegment,
  ) =
    val res = legs.indexWhere(leg =>
      leg.stops.exists(locationWithTime =>
        // TODO Make this more clear
        locationWithTime.t.localTime.value == other.start.t.localTime.value && locationWithTime.l == other.start.l,
      ),
    )
    Option.when(res != -1)(res)

  def nextAfter(
    original: RouteSegment,
  ): Option[RouteSegment] =
    for
      index <- indexOfLegThatContains(original)
      newIndex <-
        Option.when(index + 1 <= legs.size - 1)(
          index + 1,
        )
      newRoute <- legs(newIndex)
        .withSameStopsAs(original)
        .toOption
    yield RouteSegment.fromRouteLeg(newRoute)

  def nextBefore(
    original: RouteSegment,
  ): Option[RouteSegment] =
    for
      index <- indexOfLegThatContains(original)
      newIndex <-
        Option.when(index - 1 >= 0)(
          index - 1,
        )
      newRoute <- legs(newIndex)
        .withSameStopsAs(original)
        .toOption
    yield RouteSegment.fromRouteLeg(newRoute)

  val allStops: Seq[BusScheduleAtStop] =
    legs.foldLeft(Seq[BusScheduleAtStop]()) { case (acc, leg) =>
      leg.stops.foldLeft(acc) { case (innerAcc, stop) =>
        if (innerAcc.exists(_.location == stop.l))
          innerAcc.map {
            // TODO Confirm where we are getting routeName
            case BusScheduleAtStop(location, times, routeName)
                if location == stop.l =>
              BusScheduleAtStop(location,
                                times :+ stop.t,
                                leg.routeName,
              )
            case other => other
          }
        else
          innerAcc :+ BusScheduleAtStop(stop.l,
                                        Seq(stop.t),
                                        leg.routeName,
          )
      }
    }

}

object RouteWithTimes {

  def sched(
    routeName: RouteName,
    location: Location,
    routeConstructor: RouteLeg => RouteLeg,
    stopTimes: String*,
  ): RouteWithTimes = {
    val stopTimesTyped =
      stopTimes
        .flatMap(time =>
          RouteLeg(Seq(LocationWithTime(location, WallTime(time))),
                   routeName,
          ).toOption,
        )
    RouteWithTimes(
      stopTimesTyped
        .map(routeConstructor),
    )

  }

}
