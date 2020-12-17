package crestedbutte

import com.billding.time.{BusDuration, BusTime}
import crestedbutte.routes.RouteWithTimes

case class LocationWithTime(
  location: Location.Value,
  busTime: BusTime)

case class RoundTrip(
  leave: LocationWithTime,
  returnLeg: LocationWithTime)

object RoundTripCalculator {

  def calculate(
    startLocation: Location.Value,
    destination: Location.Value,
    arrivalTime: BusTime,
    timeRequiredAtDestination: BusDuration,
    leaveSchedule: RouteWithTimes,
    returnSchedule: RouteWithTimes,
  ): RoundTrip =
    ???

  def findLatestDepartureTime(arrivalTime: BusTime, leaveSchedule: RouteWithTimes):
}
