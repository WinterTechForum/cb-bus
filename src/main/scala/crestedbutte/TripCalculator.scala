package crestedbutte

import com.billding.time.{MinuteDuration, WallTime}
import crestedbutte.routes.RouteWithTimes
import pprint.PPrinter

case class TripPlannerError(
  msg: String)

case class LocationWithTime(
  location: Location,
  busTime: WallTime)

case class TripParams(
  startLocation: Location,
  destination: Location,
  arrivalTime: WallTime,
  leaveSchedule: RouteWithTimes,
  departureTime: WallTime,
  returningLaunchPoint: Location,
  returnSchedule: RouteWithTimes)

case class Trip(
  leave: RouteLeg,
  returnLeg: RouteLeg)

object TripCalculator {

  def calculate(
    tripParams: TripParams,
  ): Either[TripPlannerError, Trip] =
    calculate(
      tripParams.startLocation,
      tripParams.destination,
      tripParams.arrivalTime,
      tripParams.leaveSchedule,
      tripParams.departureTime,
      tripParams.returningLaunchPoint,
      tripParams.returnSchedule,
    )

  def calculate(
    startLocation: Location,
    destination: Location,
    arrivalTime: WallTime,
    leaveSchedule: RouteWithTimes,
    departureTime: WallTime,
    returningLaunchPoint: Location,
    returnSchedule: RouteWithTimes,
  ): Either[TripPlannerError, Trip] =
    if (arrivalTime.isAfter(departureTime))
      Left(
        TripPlannerError(
          "Departure time must be after Arrival time.",
        ),
      )
    else {
      (reducedLegStartingAt(startLocation,
                            arrivalTime,
                            destination,
                            leaveSchedule,
       ),
       reducedReturnLeg(
         LocationWithTime(returningLaunchPoint, departureTime),
         returnSchedule,
         startLocation,
       ),
      ) match {
        case (Right(startLeg), Right(returnLeg)) =>
          Right(
            Trip(
              startLeg,
              returnLeg,
            ),
          )
        case (Left(startLegError), Left(returnLegError)) =>
          Left(
            TripPlannerError(
              startLegError.msg + ", " + returnLegError.msg,
            ),
          )
        case (Left(startLegError), successfulIgnoredRoute) =>
          Left(startLegError)
        case (successfulIgnoredRoute, Left(returnLegError)) =>
          Left(returnLegError)
      }
    }

  def findLatestDepartureTime(
    arrivalTime: WallTime,
    leaveSchedule: RouteWithTimes,
  ): LocationWithTime = ???

  def reducedLegStartingAt(
    start: Location,
    arrivalTime: WallTime,
    destination: Location,
    leaveSchedule: RouteWithTimes,
  ): Either[TripPlannerError, RouteLeg] =
    findLatestDepartureLeg(arrivalTime, destination, leaveSchedule)
      .map(routeLeg =>
        routeLeg
          .trimToStartAt(start)
          .trimToEndAt(destination),
      )

  import math.Ordered.orderingToOrdered // Enables <= comparison for wall times
  def findLatestDepartureLeg(
    arrivalTime: WallTime,
    destination: Location,
    leaveSchedule: RouteWithTimes,
  ): Either[TripPlannerError, RouteLeg] =
    leaveSchedule.legs
      .findLast(leg =>
        leg.stops.exists(stop =>
          stop.location
            .matches(destination) && stop.busTime <= arrivalTime,
        ),
      )
      .toRight(
        TripPlannerError(
          "Could not find a departing leg arriving by " + arrivalTime.toDumbAmericanString,
        ),
      )

  def whenYouWouldArrive(
    start: LocationWithTime,
    schedule: RouteWithTimes,
    destination: Location,
  ): WallTime = ???

  def reducedReturnLeg(
    start: LocationWithTime,
    routeWithTimes: RouteWithTimes,
    destination: Location,
  ): Either[TripPlannerError, RouteLeg] =
    earliestReturnLeg(start, routeWithTimes)
      .map: routeLeg =>
        routeLeg
          .trimToStartAt(start.location)
          .trimToEndAt(destination)

  def earliestReturnLeg(
    target: LocationWithTime,
    routeWithTimes: RouteWithTimes,
  ): Either[TripPlannerError, RouteLeg] =
    routeWithTimes.legs
      .find(leg =>
        leg.stops.exists(stop =>
          stop.location
            .matches(
              target.location,
            ) && stop.busTime >= target.busTime,
        ),
      )
      .toRight(
        TripPlannerError(
          "Could not find a return leg after: " + target.busTime.toDumbAmericanString,
        ),
      )

  def findEarliestReturnTime(
    arrivalTime: WallTime,
    timeRequiredAtDestination: MinuteDuration,
    returnSchedule: RouteWithTimes,
  ): LocationWithTime = ???
}
