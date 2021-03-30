package crestedbutte.routes

import crestedbutte.{
  BusSchedule,
  BusScheduleAtStop,
  Location,
  LocationWithTime,
  RouteLeg,
}
import com.billding.time.WallTime

// todo I need to start building this out of RouteLegs.
//    If I correctly construct those, the express issue should go away.
case class RouteWithTimes(
  legs: Seq[RouteLeg]) {

  val allStops: Seq[BusScheduleAtStop] =
    legs.foldLeft(Seq[BusScheduleAtStop]()) {
      case (acc, leg) =>
        leg.stops.foldLeft(acc) {
          case (innerAcc, stop) => {
            if (innerAcc.exists(_.location == stop.location))
              innerAcc.map {
                case BusScheduleAtStop(location, times)
                    if (location == stop.location) =>
                  BusScheduleAtStop(location, times :+ stop.busTime)
                case other => other
              }
            else
              innerAcc :+ BusScheduleAtStop(stop.location,
                                            Seq(stop.busTime))
          }
        }
    }

  def routeLeg(
    index: Int,
  ): RouteLeg =
    RouteLeg(
      allStops
        .map(
          stop =>
            LocationWithTime(
              stop.location,
              stop.times.toList(index),
            ),
        ),
    )

  def combinedWith(
    routeWithTimes: RouteWithTimes,
  ): RouteWithTimes =
    RouteWithTimes(
      (this.legs ++ routeWithTimes.legs)
        .sortBy(_.stops.head.busTime), // TODO Is this a good place to handle the sorting?
    )

  val allInvolvedStops: Seq[Location.Value] =
    legs.head.stops.map(_.location)
}

object RouteWithTimes {

  def schedTyped(
    location: Location.Value,
    routeConstructor: RouteLeg => RouteLeg,
    stopTimes: WallTime*,
  ): RouteWithTimes =
    RouteWithTimes(
      stopTimes
        .map(
          time => RouteLeg(Seq(LocationWithTime(location, time))),
        )
        .map(routeConstructor),
    )

  def schedTyped(
    location: Location.Value,
    routeConstructor: RouteLeg => RouteLeg,
    busSchedule: BusSchedule,
  ): RouteWithTimes =
    schedTyped(location, routeConstructor, busSchedule.stopTimes: _*)

  def sched(
    location: Location.Value,
    routeConstructor: RouteLeg => RouteLeg,
    stopTimes: String*,
  ): RouteWithTimes =
    schedTyped(location,
               routeConstructor,
               stopTimes.toList.map(WallTime(_)): _*) // ugh

}
