package crestedbutte

import com.billding.time.{MinuteDuration, WallTime}
import crestedbutte.routes.RouteWithTimes
import pprint.PPrinter

case class TripPlannerError(
  msg: String)

case class RouteLeg(
  stops: Seq[LocationWithTime]) {
  assert(stops.nonEmpty, "Empty Route")

  def trimToStartAt(
    location: Location,
  ): RouteLeg =
    RouteLeg(stops.dropWhile(!_.location.matches(location)))

  def trimToEndAt(
    location: Location,
  ): RouteLeg = {
    val indexOfLastStop =
      stops.indexWhere(_.location.matches(location))
    RouteLeg(stops.take(indexOfLastStop + 1))
  }

  // Assumes non-empty
  def plus(
    location: Location,
    busDuration: MinuteDuration,
  ) =
    RouteLeg(
      stops :+ LocationWithTime(location,
                                stops.last.busTime.plus(busDuration),
      ),
    )
}

case class LocationWithTime(
  location: Location,
  busTime: WallTime)

case class RoundTripParams(
  startLocation: Location,
  destination: Location,
  arrivalTime: WallTime,
  leaveSchedule: RouteWithTimes,
  departureTime: WallTime,
  returningLaunchPoint: Location,
  returnSchedule: RouteWithTimes)

case class RoundTrip(
  leave: RouteLeg,
  returnLeg: RouteLeg)

object RoundTripCalculator {

  def calculate(
    roundTripParams: RoundTripParams,
  ): Either[TripPlannerError, RoundTrip] =
    calculate(
      roundTripParams.startLocation,
      roundTripParams.destination,
      roundTripParams.arrivalTime,
      roundTripParams.leaveSchedule,
      roundTripParams.departureTime,
      roundTripParams.returningLaunchPoint,
      roundTripParams.returnSchedule,
    )

  def calculate(
    startLocation: Location,
    destination: Location,
    arrivalTime: WallTime,
    leaveSchedule: RouteWithTimes,
    departureTime: WallTime,
    returningLaunchPoint: Location,
    returnSchedule: RouteWithTimes,
  ): Either[TripPlannerError, RoundTrip] =
    if (arrivalTime.isAfter(departureTime))
      Left(
        TripPlannerError(
          "Departure time must be after Arrival time.",
        ),
      )
    else {
      val startingLeg = reducedLegStartingAt(startLocation,
                                             arrivalTime,
                                             destination,
                                             leaveSchedule,
      )
      println("startingLeg: ")
      PPrinter.BlackWhite.pprintln(startingLeg)

      val returnLeg =
        reducedReturnLeg(
          LocationWithTime(returningLaunchPoint, departureTime),
          returnSchedule,
          startLocation,
        )

      println("returnLeg: ")
      PPrinter.BlackWhite.pprintln(returnLeg)
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
            RoundTrip(
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
                      ): Either[TripPlannerError, RouteLeg] = {
    earliestReturnLeg(start, routeWithTimes)
      .map: routeLeg =>
        routeLeg
          .trimToStartAt(start.location)
          .trimToEndAt(destination)
  }

  def earliestReturnLeg(
    target: LocationWithTime,
    routeWithTimes: RouteWithTimes,
  ): Either[TripPlannerError, RouteLeg] =
    routeWithTimes.legs
      .find(leg =>
        leg.stops.exists(stop =>
          stop.location
            .matches(target.location) && stop.busTime >= target.busTime,
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
