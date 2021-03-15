package crestedbutte

import com.billding.time.{BusDuration, BusTime}
import crestedbutte.routes.RouteWithTimes

case class TripPlannerError(
  msg: String)

case class RouteLeg(
  stops: Seq[LocationWithTime]) {
  assert(stops.nonEmpty, "Empty Route")

  def trimToStartAt(
    location: Location.Value,
  ): RouteLeg =
    RouteLeg(stops.dropWhile(_.location != location))

  def trimToEndAt(
    location: Location.Value,
  ): RouteLeg = {
    val indexOfLastStop =
      stops.indexWhere(_.location == location)
    RouteLeg(stops.take(indexOfLastStop + 1))
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

case class RoundTripParams(
  startLocation: Location.Value,
  destination: Location.Value,
  arrivalTime: BusTime,
  leaveSchedule: RouteWithTimes,
  timeRequiredAtDestination: BusDuration,
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
      roundTripParams.timeRequiredAtDestination,
      roundTripParams.returningLaunchPoint,
      roundTripParams.returnSchedule,
    )

  def calculate(
    startLocation: Location.Value,
    destination: Location.Value,
    arrivalTime: BusTime,
    leaveSchedule: RouteWithTimes,
    timeRequiredAtDestination: BusDuration,
    returningLaunchPoint: Location.Value,
    returnSchedule: RouteWithTimes,
  ): Either[TripPlannerError, RoundTrip] =
    (reducedLegStartingAt(startLocation,
                          arrivalTime,
                          destination,
                          leaveSchedule),
     reducedReturnLeg(
       LocationWithTime(returningLaunchPoint,
                        arrivalTime.plus(timeRequiredAtDestination)),
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

  def findLatestDepartureTime(
    arrivalTime: BusTime,
    leaveSchedule: RouteWithTimes,
  ): LocationWithTime = ???

  def reducedLegStartingAt(
    start: Location.Value,
    arrivalTime: BusTime,
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
    arrivalTime: BusTime,
    destination: Location.Value,
    leaveSchedule: RouteWithTimes,
  ): Either[TripPlannerError, RouteLeg] =
    leaveSchedule.legs
      .findLast(
        leg =>
          leg.stops.exists(
            stop =>
              stop.location == destination && BusTime.busTimeOrdering
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
  ): BusTime = ???

  def reducedReturnLeg(
    target: LocationWithTime,
    routeWithTimes: RouteWithTimes,
    destination: Location.Value,
  ): Either[TripPlannerError, RouteLeg] = {
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
              stop.location == target.location && BusTime.busTimeOrdering
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
    arrivalTime: BusTime,
    timeRequiredAtDestination: BusDuration,
    returnSchedule: RouteWithTimes,
  ): LocationWithTime = ???
}
