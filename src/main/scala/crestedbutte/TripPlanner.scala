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

object TripPlanner {

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
      (routeWithTimeOfArrival(startLocation,
                              arrivalTime,
                              destination,
                              leaveSchedule,
       ),
        routeStartingAfter(
         returningLaunchPoint,
         departureTime,
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

  import math.Ordered.orderingToOrdered // Enables <= comparison for wall times
  def routeWithTimeOfArrival(
    start: Location,
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
      .map(routeLeg =>
        routeLeg
          .trimToStartAt(start)
          .trimToEndAt(destination),
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

  def routeStartingAfter(
                          start: Location,
                          arrivalTime: WallTime,
//                          start: LocationWithTime,
                         routeWithTimes: RouteWithTimes,
                         destination: Location,
  ): Either[TripPlannerError, RouteLeg] =
    routeWithTimes.legs
      .find(leg =>
        leg.stops.exists(stop =>
          stop.location
            .matches(
              start,
            ) && stop.busTime >= arrivalTime,
        ),
      )

      .map: routeLeg =>
          routeLeg
            .trimToStartAt(start)
            .trimToEndAt(destination)
      .toRight(
        TripPlannerError(
          "Could not find a return leg after: " + arrivalTime.toDumbAmericanString,
        ),
      )

  def findEarliestReturnTime(
    arrivalTime: WallTime,
    timeRequiredAtDestination: MinuteDuration,
    returnSchedule: RouteWithTimes,
  ): LocationWithTime = ???
}
