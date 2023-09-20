package crestedbutte

import zio.*
import zio.test.*
import com.billding.time.WallTime
import routes.RouteWithTimes
import routes.*

object TripCalculatorTest extends ZIOSpecDefault:
  def spec =
    suite("TripCalculatorTest")(
      suite("startingLeg") (
        test("departure leg tests") {
          val start: Location = Location.BrushCreek
          val arrivalTime: WallTime = WallTime("07:35")
          val destination: Location = Location.RecCenter
          val leaveSchedule: RouteWithTimes = RtaSouthbound.fullSchedule.routeWithTimes
          val results =
            RoundTripCalculator.reducedLegStartingAt(start, arrivalTime, destination, leaveSchedule).right.get
          assertTrue(results.stops.head.busTime == WallTime("06:51"))
        },
        test("departure leg tests 2") {
          val results =
            RoundTripCalculator.reducedLegStartingAt(
              start = Location.CBSouth,
              arrivalTime = WallTime("20:00"),
              destination = Location.RecCenter,
              leaveSchedule = RtaSouthbound.fullSchedule.routeWithTimes
            ).getOrElse(???)
          assertTrue(results.stops.head.busTime == WallTime("19:10"))
        },

        test("Northbound to CB South in the morning, honoring the huge express bus void") {
          val results =
            RoundTripCalculator.reducedLegStartingAt(
              start = Location.GunnisonCommunitySchools,
              arrivalTime = WallTime("09:00"),
              destination = Location.CBSouth,
              leaveSchedule = RtaNorthbound.fullSchedule.routeWithTimes
            ).getOrElse(???)
          val startingPoint = results.stops.head.busTime
          assertTrue(startingPoint == WallTime("08:05"))
        }
      ),

      suite ("returnLeg") {
//        test("departure leg tests") {
//          val start: Location = Location.BrushCreek
//          val earliestDepartureTime: WallTime = WallTime("08:00")
//          val destination: Location = Location.RecCenter
//          val leaveSchedule: RouteWithTimes = RtaSouthbound.fullSchedule.routeWithTimes
//          val results =
//            RoundTripCalculator.reducedReturnLeg(LocationWithTime(start, earliestDepartureTime), leaveSchedule, destination)
//              .getOrElse(???)
//          assertTrue(results.stops.head.busTime == WallTime("08:22") &&
//            results.stops.last.busTime == WallTime("08:56"))
//        },
        test("departure leg tests 2") {
          val start: Location = Location.Riverland
          val earliestDepartureTime: WallTime = WallTime("15:00")
          val destination: Location = Location.Almont
          val leaveSchedule: RouteWithTimes = RtaSouthbound.fullSchedule.routeWithTimes
          val results =
            RoundTripCalculator.reducedReturnLeg(LocationWithTime(start, earliestDepartureTime), leaveSchedule, destination).right.get
          assertTrue(results.stops.head.busTime == WallTime("15:28") &&
            results.stops.last.busTime == WallTime("15:42"))
        } @@ TestAspect.ignore

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
    )

