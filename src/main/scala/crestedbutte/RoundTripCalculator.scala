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
  ): RoundTrip =
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
  ): RoundTrip =
    RoundTrip(
      reducedLegStartingAt(startLocation,
                           arrivalTime,
                           destination,
                           leaveSchedule),
      reducedReturnLeg(
        LocationWithTime(returningLaunchPoint,
                         arrivalTime.plus(timeRequiredAtDestination)),
        returnSchedule,
        startLocation,
      ),
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
  ): RouteLeg =
    leaveSchedule.legs
      .findLast(
        leg =>
          leg.stops.exists(
            stop =>
              stop.location == destination && BusTime.busTimeOrdering
                .compare(stop.busTime, arrivalTime) <= 0, // todo ugh. bad int math.
          ),
      )
      .getOrElse(throw new RuntimeException("D'oh!"))

  def whenYouWouldArrive(
    start: LocationWithTime,
    schedule: RouteWithTimes,
    destination: Location.Value,
  ): BusTime = ???

  def reducedReturnLeg(
    target: LocationWithTime,
    routeWithTimes: RouteWithTimes,
    destination: Location.Value,
  ) =
    earliestReturnLeg(target, routeWithTimes)
      .trimToStartAt(target.location)
      .trimToEndAt(destination)

  def earliestReturnLeg(
    target: LocationWithTime,
    routeWithTimes: RouteWithTimes,
  ): RouteLeg =
    routeWithTimes.legs
      .find(
        leg =>
          leg.stops.exists(
            stop =>
              stop.location == target.location && BusTime.busTimeOrdering
                .compare(stop.busTime, target.busTime) >= 0, // todo ugh. bad int math.
          ),
      )
      .getOrElse(throw new RuntimeException("D'oh!"))

  def findEarliestReturnTime(
    arrivalTime: BusTime,
    timeRequiredAtDestination: BusDuration,
    returnSchedule: RouteWithTimes,
  ): LocationWithTime = ???
}
