package crestedbutte

import com.billding.time.{BusDuration, BusTime}
import crestedbutte.routes.RouteWithTimes

case class RouteLeg(
  stops: Seq[LocationWithTime])

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
      findLatestDepartureLeg(arrivalTime, destination, leaveSchedule).stops match {
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

  def findLatestDepartureLeg(
    arrivalTime: BusTime,
    destination: Location.Value,
    leaveSchedule: RouteWithTimes,
  ): RouteLeg = {
    val targetIndexToAssembleRoute = {
      leaveSchedule.allStops.zipWithIndex
        .find {
          case (busScheduleAtStop: BusScheduleAtStop, idx) =>
            busScheduleAtStop.location == destination
        }
        .map {
          case (busScheduleAtStop: BusScheduleAtStop, idx) =>
            busScheduleAtStop.times.lastIndexWhere(
              _.isBeforeOrEqualTo(arrivalTime),
            )
        }
        .getOrElse(throw new RuntimeException("D'oh!"))

    }
    RouteLeg(
      leaveSchedule.allStops
        .map(
          stop =>
            LocationWithTime(stop.location,
                             stop.times(targetIndexToAssembleRoute)),
        ),
    )
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
