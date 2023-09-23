package crestedbutte

import com.billding.time.{MinuteDuration, WallTime}
import crestedbutte.TripParamZ.StartingAfter
import crestedbutte.routes.RouteWithTimes
import pprint.PPrinter

case class TripPlannerError(
  msg: String)

case class LocationWithTime(
  location: Location,
  busTime: WallTime)

enum TripParamZ {
  case StartingAfter(
    start: Location,
    arrivalTime: WallTime, // TODO rename to earliestDeparture
    routeWithTimes: RouteWithTimes,
    destination: Location)

  case ArrivingBy(
    start: Location,
    arrivalTime: WallTime,
    destination: Location,
    leaveSchedule: RouteWithTimes)

  import math.Ordered.orderingToOrdered // Enables <= comparison for wall times
  def evaluate(
  ): Either[TripPlannerError, RouteLeg] =
    this match
      case s: StartingAfter =>
        s.routeWithTimes.legs
          .find(leg =>
            leg.stops.exists(stop =>
              stop.location
                .matches(
                  s.start,
                ) && stop.busTime >= s.arrivalTime,
            ),
          )
          .map: routeLeg =>
            routeLeg
              .trimToStartAt(s.start)
              .trimToEndAt(s.destination)
          .toRight(
            TripPlannerError(
              "Could not find a return leg after: " + s.arrivalTime.toDumbAmericanString,
            ),
          )
      case b: ArrivingBy =>
        b.leaveSchedule.legs
          .findLast(leg =>
            leg.stops.exists(stop =>
              stop.location
                .matches(
                  b.destination,
                ) && stop.busTime <= b.arrivalTime,
            ),
          )
          .map(routeLeg =>
            routeLeg
              .trimToStartAt(b.start)
              .trimToEndAt(b.destination),
          )
          .toRight(
            TripPlannerError(
              "Could not find a departing leg arriving by " + b.arrivalTime.toDumbAmericanString,
            ),
          )

}

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
      (TripParamZ
         .ArrivingBy(startLocation,
                     arrivalTime,
                     destination,
                     leaveSchedule,
         )
         .evaluate(),
       TripParamZ
         .StartingAfter(
           returningLaunchPoint,
           departureTime,
           returnSchedule,
           startLocation,
         )
         .evaluate(),
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

}
