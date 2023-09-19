package crestedbutte

import com.billding.time.{MinuteDuration, WallTime}
import crestedbutte.routes.{RouteWithTimes, RtaNorthbound, RtaSouthbound}
import utest.{TestSuite, Tests}
import utest._

object RoundTripCalculatorTest extends TestSuite {
  val tests = Tests {
    test("startingLeg") {
      test("departure leg tests") {

        val start: Location = Location.BrushCreek
        val arrivalTime: WallTime = WallTime("07:35")
        val destination: Location = Location.RecCenter
        val leaveSchedule: RouteWithTimes = RtaSouthbound.fullSchedule.routeWithTimes
        val results =
          RoundTripCalculator.reducedLegStartingAt(start, arrivalTime, destination, leaveSchedule).right.get
        assert(results.stops.head.busTime == WallTime("06:52"))
      }
      test("departure leg tests 2") {

        val start: Location = Location.CBSouth
        val arrivalTime: WallTime = WallTime("20:00")
        val destination: Location = Location.RecCenter
        val leaveSchedule: RouteWithTimes = RtaSouthbound.fullSchedule.routeWithTimes
        val results =
          RoundTripCalculator.reducedLegStartingAt(start, arrivalTime, destination, leaveSchedule).right.get
        assert(results.stops.head.busTime == WallTime("18:35"))
      }

      test("Northbound to CB South in the morning, honoring the huge express bus void") {
        val start: Location = Location.GunnisonCommunitySchools
        val arrivalTime: WallTime = WallTime("09:00")
        val destination: Location = Location.CBSouth
        val leaveSchedule: RouteWithTimes = RtaNorthbound.fullSchedule.routeWithTimes
        val results =
          RoundTripCalculator.reducedLegStartingAt(start, arrivalTime, destination, leaveSchedule).right.get
        assert(results.stops.head.busTime == WallTime("06:00"))
      }
    }

    test("returnLeg") {
      test("departure leg tests") {

        val start: Location = Location.BrushCreek
        val earliestDepartureTime: WallTime = WallTime("08:00")
        val destination: Location = Location.RecCenter
        val leaveSchedule: RouteWithTimes = RtaSouthbound.fullSchedule.routeWithTimes
        val results =
          RoundTripCalculator.reducedReturnLeg(LocationWithTime( start, earliestDepartureTime), leaveSchedule, destination)
            .getOrElse(???)
        assert(results.stops.head.busTime == WallTime("08:22"))
        assert(results.stops.last.busTime == WallTime("08:56"))
      }
      test("departure leg tests 2") {

        val start: Location = Location.Riverland
        val earliestDepartureTime: WallTime = WallTime("15:00")
        val destination: Location = Location.Almont
        val leaveSchedule: RouteWithTimes = RtaSouthbound.fullSchedule.routeWithTimes
        val results =
          RoundTripCalculator.reducedReturnLeg(LocationWithTime(start, earliestDepartureTime), leaveSchedule, destination).right.get
        assert(results.stops.head.busTime == WallTime("15:28"))
        assert(results.stops.last.busTime == WallTime("15:42"))
      }

      test("Northbound from CB South in the morning, honoring the huge express bus void") {
        val start: Location = Location.CBSouth
        val earliestDepartureTime: WallTime = WallTime("06:45")
        val destination: Location = Location.Riverland
        val leaveSchedule: RouteWithTimes = RtaNorthbound.fullSchedule.routeWithTimes
        val results =
          RoundTripCalculator.reducedReturnLeg(LocationWithTime(start, earliestDepartureTime), leaveSchedule, destination).right.get
        assert(results.stops.head.busTime == WallTime("09:42"))
      }
    }
//    test("Full Round Trip" ) {
//      RoundTripCalculator.calculate(
//        startLocation = Location.BrushCreek,
//        destination = Location.RecCenter,
//        arrivalTime = WallTime("18:30"),
//        leaveSchedule = RtaSouthbound.fullSchedule.routeWithTimes,
//        timeRequiredAtDestination = MinuteDuration.ofMinutes(60),  // How long I need to be there
//        returningLaunchPoint = Location.SpencerAndHighwayOneThirtyFive, // Where I'll be catching the bus home
//        returnSchedule = RtaNorthbound.fullSchedule.routeWithTimes)
//    }
  }
}
