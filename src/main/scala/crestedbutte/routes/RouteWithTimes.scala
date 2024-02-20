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

  def indexOfLegThatContains(
    other: RouteLeg,
  ) =
    val res = legs.indexWhere(leg =>
      leg.stops.exists(locationWithTime =>
        locationWithTime.busTime.localTime.value == other.stops.head.busTime.localTime.value && locationWithTime.location == other.stops.head.location,
      ),
    )
    Option.when(res != -1)(res)

  def nextAfter(
    original: RouteLeg,
  ): Option[RouteLeg] =
    for
      index <- indexOfLegThatContains(original)
      newIndex <-
        Option.when(index + 1 <= legs.size - 1)(
          index + 1,
        )
      newRoute <- legs(newIndex)
        .withSameStopsAs(original)
        .toOption
    yield newRoute

  def nextBefore(
    original: RouteLeg,
  ): Option[RouteLeg] =
    for
      index <- indexOfLegThatContains(original)
      newIndex <-
        Option.when(index - 1 >= 0)(
          index - 1,
        )
      newRoute <- legs(newIndex)
        .withSameStopsAs(original)
        .toOption
    yield newRoute

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

  def firstRouteLeg(
  ): RouteLeg =
    RouteLeg(
      allStops
        .map(stop =>
          LocationWithTime(
            stop.location,
            stop.times.toList(0),
          ),
        ),
      allStops.head.routeName, // TODO Unsafe
    ).getOrElse(throw new IllegalStateException("No stops in route"))

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
        .flatMap(time =>
          RouteLeg(Seq(LocationWithTime(location, time)),
                   routeName,
          ).toOption,
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
