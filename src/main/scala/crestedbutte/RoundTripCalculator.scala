package crestedbutte

import com.billding.time.{BusDuration, BusTime}
import crestedbutte.routes.RouteWithTimes

case class RouteLeg(
  stops: Seq[LocationWithTime]) {

  def trimToStartAt(
    location: Location.Value,
  ): RouteLeg =
    RouteLeg(stops.dropWhile(_.location != location))

  def trimToEndAt(
    location: Location.Value,
  ): RouteLeg = {
    val indexOf2ndToLastStop =
      stops.lastIndexWhere(_.location != location)
    RouteLeg(stops.take(indexOf2ndToLastStop))
  }

  // Assumes non-empty
  def plus(
    location: Location.Value,
    busDuration: BusDuration,
  ) =
    RouteLeg(
      stops :+ LocationWithTime(location,
                                stops.last.busTime.plus(busDuration)),
    )
}

case class LocationWithTime(
  location: Location.Value,
  busTime: BusTime)

case class RoundTrip(
  leave: RouteLeg,
  returnLeg: RouteLeg)

object RoundTripCalculator {

  def calculate(
    startLocation: Location.Value,
    destination: Location.Value,
    arrivalTime: BusTime,
    timeRequiredAtDestination: BusDuration,
    leaveSchedule: RouteWithTimes,
    returnSchedule: RouteWithTimes,
  ): RoundTrip =
    RoundTrip(
      reducedLegStartingAt(startLocation,
                           arrivalTime,
                           destination,
                           leaveSchedule).stops match {
        case (begin :: middleStops) :+ end => ???
        case _                             => throw new RuntimeException("Can we prevent this?")
      },
      earliestReturnLeg(
        LocationWithTime(destination,
                         arrivalTime.plus(timeRequiredAtDestination)),
        returnSchedule,
      ).stops match {
        case (begin :: middleStops) :+ end => ???
        case _                             => throw new RuntimeException("Can we prevent this?")
      },
    )

  def findLatestDepartureTime(
    arrivalTime: BusTime,
    leaveSchedule: RouteWithTimes,
  ): LocationWithTime = ???

  def reducedLegStartingAt(
    start: Location.Value,
    arrivalTime: BusTime,
    destination: Location.Value,
    leaveSchedule: RouteWithTimes,
  ): RouteLeg =
    findLatestDepartureLeg(arrivalTime, destination, leaveSchedule)
      .trimToStartAt(start)
      .trimToEndAt(destination)

  def findLatestDepartureLeg(
    arrivalTime: BusTime,
    destination: Location.Value,
    leaveSchedule: RouteWithTimes,
  ): RouteLeg = {
    val targetIndexToAssembleRoute =
      leaveSchedule.allStops.zipWithIndex
        .find {
          case (busScheduleAtStop: BusScheduleAtStop, idx) =>
            busScheduleAtStop.location == destination
        }
        .map {
          case (busScheduleAtStop: BusScheduleAtStop, idx) =>
            busScheduleAtStop.times.lastIndexWhere(
              BusTime.busTimeOrdering.compare(_, arrivalTime) <= 0,
            )
        }
        .getOrElse(throw new RuntimeException("D'oh!"))

    println("targetIndex: " + targetIndexToAssembleRoute)
    leaveSchedule.routeLeg(targetIndexToAssembleRoute)
  }

  def whenYouWouldArrive(
    start: LocationWithTime,
    schedule: RouteWithTimes,
    destination: Location.Value,
  ): BusTime = ???

  def earliestReturnLeg(
    target: LocationWithTime,
    routeWithTimes: RouteWithTimes,
  ): RouteLeg = ???

  def legOfJourneyThatContains(
    target: LocationWithTime,
    routeWithTimes: RouteWithTimes,
  ): RouteLeg = ???

  def findEarliestReturnTime(
    arrivalTime: BusTime,
    timeRequiredAtDestination: BusDuration,
    returnSchedule: RouteWithTimes,
  ): LocationWithTime = ???
}
