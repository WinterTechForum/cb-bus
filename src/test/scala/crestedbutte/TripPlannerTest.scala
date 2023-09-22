package crestedbutte

import zio.*
import zio.test.*
import com.billding.time.WallTime
import routes.RouteWithTimes
import routes.*

object TripPlannerTest extends ZIOSpecDefault:
  def spec =
    suite("TripPlannerTest")(
      suite("startingLeg") (
        test("departure leg tests"):
          val results =
            TripParamz.ArrivingBy(
              start = Location.BrushCreek,
              arrivalTime = WallTime("07:35"),
              destination = Location.RecCenter,
              leaveSchedule = RtaSouthbound.fullSchedule.routeWithTimes
          ).evaluate().getOrElse(???)
          assertTrue:
            results.stops.head.busTime == WallTime("06:51")
        ,
        test("departure leg tests 2"):
          val results =
            TripParamz.ArrivingBy(
              start = Location.CBSouth,
              arrivalTime = WallTime("20:00"),
              destination = Location.RecCenter,
              leaveSchedule = RtaSouthbound.fullSchedule.routeWithTimes
            ).evaluate().getOrElse(???)
          assertTrue:
            results.stops.head.busTime == WallTime("19:10")
        ,

        test("Northbound to CB South in the morning, honoring the huge express bus void"):
          val results =
            TripParamz.ArrivingBy(
              start = Location.GunnisonCommunitySchools,
              arrivalTime = WallTime("09:00"),
              destination = Location.CBSouth,
              leaveSchedule = RtaNorthbound.fullSchedule.routeWithTimes
            ).evaluate().getOrElse(???)
          val startingPoint = results.stops.head.busTime
          assertTrue(startingPoint == WallTime("08:05"))
      ),

      suite ("returnLeg")(
        test("departure leg tests") {
          val results =
              TripParamz.StartingAfter(
                start = Location.BrushCreek,
                arrivalTime = WallTime("08:00"),
                routeWithTimes = RtaSouthbound.fullSchedule.routeWithTimes,
                destination = Location.RecCenter
            ).evaluate().getOrElse(???)
          assertTrue(results.stops.head.busTime == WallTime("08:26") &&
            results.stops.last.busTime == WallTime("09:01"))
        },
        test("departure leg tests 2") {
          val results =
              TripParamz.StartingAfter(
                start = Location.Riverland,
                arrivalTime = WallTime("15:00"),
                routeWithTimes = RtaSouthbound.fullSchedule.routeWithTimes,
                destination = Location.Almont
            ).evaluate().right.get
          assertTrue(results.stops.head.busTime == WallTime("15:42") &&
            results.stops.last.busTime == WallTime("16:04"))
        }

      )
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

