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
    location: Location.Value,
  ): RouteLeg =
    RouteLeg(stops.dropWhile(!_.location.matches(location)))

  def trimToEndAt(
    location: Location.Value,
  ): RouteLeg = {
    val indexOfLastStop =
      stops.indexWhere(_.location.matches(location))
    println("should end at stop: " + location)
    println("indexOfLastStop: " + indexOfLastStop)
    PPrinter.BlackWhite.pprintln(stops)
    RouteLeg(stops.take(indexOfLastStop + 1))
  }

  // Assumes non-empty
  def plus(
    location: Location.Value,
    busDuration: MinuteDuration,
  ) =
    RouteLeg(
      stops :+ LocationWithTime(location,
                                stops.last.busTime.plus(busDuration)),
    )
}

case class LocationWithTime(
  location: Location.Value,
  busTime: WallTime)

case class RoundTripParams(
  startLocation: Location.Value,
  destination: Location.Value,
  arrivalTime: WallTime,
  leaveSchedule: RouteWithTimes,
  departureTime: WallTime,
  returningLaunchPoint: Location.Value,
  returnSchedule: RouteWithTimes,
)

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
    startLocation: Location.Value,
    destination: Location.Value,
    arrivalTime: WallTime,
    leaveSchedule: RouteWithTimes,
    departureTime: WallTime,
    returningLaunchPoint: Location.Value,
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
                                             leaveSchedule)
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
                            leaveSchedule),
       reducedReturnLeg(
         LocationWithTime(returningLaunchPoint, departureTime),
         returnSchedule,
         startLocation,
       )) match {
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
    start: Location.Value,
    arrivalTime: WallTime,
    destination: Location.Value,
    leaveSchedule: RouteWithTimes,
  ): Either[TripPlannerError, RouteLeg] =
    findLatestDepartureLeg(arrivalTime, destination, leaveSchedule)
      .map(
        routeLeg =>
          routeLeg
            .trimToStartAt(start)
            .trimToEndAt(destination),
      )

  def findLatestDepartureLeg(
    arrivalTime: WallTime,
    destination: Location.Value,
    leaveSchedule: RouteWithTimes,
  ): Either[TripPlannerError, RouteLeg] =
    leaveSchedule.legs
      .findLast(
        leg =>
          leg.stops.exists(
            stop =>
              stop.location
                .matches(destination) && WallTime.ordering
                .compare(stop.busTime, arrivalTime) <= 0, // todo ugh. bad int math.
          ),
      )
      .map(Right(_))
      .getOrElse(
        Left(
          TripPlannerError(
            "Could not find a departing leg arriving by " + arrivalTime.toDumbAmericanString,
          ),
        ),
      )

  def whenYouWouldArrive(
    start: LocationWithTime,
    schedule: RouteWithTimes,
    destination: Location.Value,
  ): WallTime = ???

  def reducedReturnLeg(
    target: LocationWithTime,
    routeWithTimes: RouteWithTimes,
    destination: Location.Value,
  ): Either[TripPlannerError, RouteLeg] = {
    println("Return leg starts from: " + target)
    println("and ends at: " + destination)
    val returnLeg: Either[TripPlannerError, RouteLeg] =
      earliestReturnLeg(target, routeWithTimes)

    returnLeg.map {
      routeLeg =>
        routeLeg
          .trimToStartAt(target.location)
          .trimToEndAt(destination)
    }
  }

  def earliestReturnLeg(
    target: LocationWithTime,
    routeWithTimes: RouteWithTimes,
  ): Either[TripPlannerError, RouteLeg] =
    routeWithTimes.legs
      .find(
        leg =>
          leg.stops.exists(
            stop =>
              stop.location
                .matches(target.location) && WallTime.ordering
                .compare(stop.busTime, target.busTime) >= 0, // todo ugh. bad int math.
          ),
      )
      .map(Right(_))
      .getOrElse(
        Left(
          TripPlannerError(
            "Could not find a return leg after: " + target.busTime.toDumbAmericanString,
          ),
        ),
      )

  def findEarliestReturnTime(
    arrivalTime: WallTime,
    timeRequiredAtDestination: MinuteDuration,
    returnSchedule: RouteWithTimes,
  ): LocationWithTime = ???
}
