package crestedbutte.routes

import crestedbutte.{
  BusSchedule,
  BusScheduleAtStop,
  ComponentName,
  Location,
  LocationWithTime,
  RouteLeg,
}
import com.billding.time.WallTime

case class RouteWithTimes(
  legs: Seq[RouteLeg]) {
  
  def indexOfLegThatContains(other: RouteLeg) =
    val res = legs.indexWhere(leg => leg.stops.exists(locationWithTime => locationWithTime.busTime.minutes == other.stops.head.busTime.minutes && locationWithTime.location == other.stops.head.location))
    Option.when(res != -1)(res)

  def nextAfter(original: RouteLeg): Option[RouteLeg] =
      indexOfLegThatContains(original)
        .flatMap(index =>
          Option.when(index + 1 <= legs.size - 1)(
            legs(index + 1)
              .withSameStopsAs(original)
          )
        )

    
  def nextBefore(original: RouteLeg): Option[RouteLeg] =
      indexOfLegThatContains(original)
        .flatMap(index =>
          Option.when(index - 1 >= 0)(
            legs(index - 1)
              .withSameStopsAs(original)
          )
        )


  val allStops: Seq[BusScheduleAtStop] =
    legs.foldLeft(Seq[BusScheduleAtStop]()) { case (acc, leg) =>
      leg.stops.foldLeft(acc) { case (innerAcc, stop) =>
        if (innerAcc.exists(_.location == stop.location))
          innerAcc.map {
            // TODO Confirm where we are getting routeName
            case BusScheduleAtStop(location, times, routeName)
                if location == stop.location =>
              BusScheduleAtStop(location,
                                times :+ stop.busTime,
                                leg.routeName,
              )
            case other => other
          }
        else
          innerAcc :+ BusScheduleAtStop(stop.location,
                                        Seq(stop.busTime),
                                        leg.routeName,
          )
      }
    }

  def routeLeg(
    index: Int,
  ): RouteLeg =
    RouteLeg(
      allStops
        .map(stop =>
          LocationWithTime(
            stop.location,
            stop.times.toList(index),
          ),
        ),
      allStops.head.routeName, // TODO Unsafe
    )

  def combinedWith(
    routeWithTimes: RouteWithTimes,
  ): RouteWithTimes =
    RouteWithTimes(
      (this.legs ++ routeWithTimes.legs)
        .sortBy(
          _.stops.head.busTime,
        ), // TODO Is this a good place to handle the sorting?
    )

  val allInvolvedStops: Seq[Location] =
    legs.head.stops.map(_.location)
}

object RouteWithTimes {

  def schedTyped(
    routeName: ComponentName,
    location: Location,
    routeConstructor: RouteLeg => RouteLeg,
    stopTimes: WallTime*,
  ): RouteWithTimes =
    RouteWithTimes(
      stopTimes
        .map(time =>
          RouteLeg(Seq(LocationWithTime(location, time)), routeName),
        )
        .map(routeConstructor),
    )

  def schedTyped(
    routeName: ComponentName,
    location: Location,
    routeConstructor: RouteLeg => RouteLeg,
    busSchedule: BusSchedule,
  ): RouteWithTimes =
    schedTyped(routeName,
               location,
               routeConstructor,
               busSchedule.stopTimes: _*,
    )

  def sched(
    routeName: ComponentName,
    location: Location,
    routeConstructor: RouteLeg => RouteLeg,
    stopTimes: String*,
  ): RouteWithTimes =
    schedTyped(
      routeName,
      location,
      routeConstructor,
      stopTimes.toList.map(WallTime(_)): _*,
    ) // ugh

}
