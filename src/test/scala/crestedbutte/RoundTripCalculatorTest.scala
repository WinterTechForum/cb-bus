package crestedbutte

import com.billding.time.{BusDuration, BusTime}
import crestedbutte.routes.{RouteWithTimes, RtaNorthbound, RtaSouthbound}
import utest.{TestSuite, Tests}
import utest._

object RoundTripCalculatorTest extends TestSuite {
  val tests = Tests {
    test("startingLeg") {
      test("departure leg tests") {

        val start: Location.Value = Location.BrushCreek
        val arrivalTime: BusTime = BusTime("07:35")
        val destination: Location.Value = Location.RecCenter
        val leaveSchedule: RouteWithTimes = RtaSouthbound.fullSchedule.routeWithTimes
        val results =
          RoundTripCalculator.reducedLegStartingAt(start, arrivalTime, destination, leaveSchedule)
        assert(results.stops.head.busTime == BusTime("06:52"))
      }
      test("departure leg tests 2") {

        val start: Location.Value = Location.CBSouth
        val arrivalTime: BusTime = BusTime("20:00")
        val destination: Location.Value = Location.RecCenter
        val leaveSchedule: RouteWithTimes = RtaSouthbound.fullSchedule.routeWithTimes
        val results =
          RoundTripCalculator.reducedLegStartingAt(start, arrivalTime, destination, leaveSchedule)
        assert(results.stops.head.busTime == BusTime("18:35"))
      }

      test("Northbound to CB South in the morning, honoring the huge express bus void") {
        val start: Location.Value = Location.GunnisonCommunitySchools
        val arrivalTime: BusTime = BusTime("09:00")
        val destination: Location.Value = Location.CBSouth
        val leaveSchedule: RouteWithTimes = RtaNorthbound.fullSchedule.routeWithTimes
        val results =
          RoundTripCalculator.reducedLegStartingAt(start, arrivalTime, destination, leaveSchedule)
        assert(results.stops.head.busTime == BusTime("06:00"))
      }
    }

    test("returnLeg") {
      test("departure leg tests") {

        val start: Location.Value = Location.BrushCreek
        val earliestDepartureTime: BusTime = BusTime("08:00")
        val destination: Location.Value = Location.RecCenter
        val leaveSchedule: RouteWithTimes = RtaSouthbound.fullSchedule.routeWithTimes
        val results =
          RoundTripCalculator.reducedReturnLeg(LocationWithTime( start, earliestDepartureTime), leaveSchedule, destination)
        assert(results.stops.head.busTime == BusTime("08:22"))
        assert(results.stops.last.busTime == BusTime("08:56"))
      }
      test("departure leg tests 2") {

        val start: Location.Value = Location.Riverland
        val earliestDepartureTime: BusTime = BusTime("15:00")
        val destination: Location.Value = Location.Almont
        val leaveSchedule: RouteWithTimes = RtaSouthbound.fullSchedule.routeWithTimes
        val results =
          RoundTripCalculator.reducedReturnLeg(LocationWithTime(start, earliestDepartureTime), leaveSchedule, destination)
        assert(results.stops.head.busTime == BusTime("15:28"))
        assert(results.stops.last.busTime == BusTime("15:42"))
      }

      test("Northbound from CB South in the morning, honoring the huge express bus void") {
        val start: Location.Value = Location.CBSouth
        val earliestDepartureTime: BusTime = BusTime("06:45")
        val destination: Location.Value = Location.Riverland
        val leaveSchedule: RouteWithTimes = RtaNorthbound.fullSchedule.routeWithTimes
        val results =
          RoundTripCalculator.reducedReturnLeg(LocationWithTime(start, earliestDepartureTime), leaveSchedule, destination)
        assert(results.stops.head.busTime == BusTime("09:42"))
      }
    }
    test("Full Round Trip" ) {
      RoundTripCalculator.calculate(
        Location.BrushCreek,
        Location.RecCenter,
        BusTime("18:30"), // When I need to arrive
        RtaSouthbound.fullSchedule.routeWithTimes,
        BusDuration.ofMinutes(60),  // How long I need to be there
        Location.SpencerAndHighwayOneThirtyFive,
        RtaNorthbound.fullSchedule.routeWithTimes)
    }
  }
}
