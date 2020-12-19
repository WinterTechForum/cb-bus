package crestedbutte

import com.billding.time.BusTime
import crestedbutte.routes.{RouteWithTimes, RtaSouthbound}
import utest.{TestSuite, Tests}
import utest._

object RoundTripCalculatorTest extends TestSuite {
  val tests = Tests {
    test("departure leg tests") {

      val start: Location.Value = Location.BrushCreek
      val arrivalTime: BusTime = BusTime("07:35")
      val destination: Location.Value = Location.RecCenter
      val leaveSchedule: RouteWithTimes = RtaSouthbound.fullSchedule.routeWithTimes
      val results =
      RoundTripCalculator.reducedLegStartingAt(start, arrivalTime, destination, leaveSchedule)
      assert( results.stops.head.busTime == BusTime("06:52"))
    }
    test("departure leg tests 2") {

      val start: Location.Value = Location.CBSouth
      val arrivalTime: BusTime = BusTime("20:00")
      val destination: Location.Value = Location.RecCenter
      val leaveSchedule: RouteWithTimes = RtaSouthbound.fullSchedule.routeWithTimes
      val results =
        RoundTripCalculator.reducedLegStartingAt(start, arrivalTime, destination, leaveSchedule)
      assert( results.stops.head.busTime == BusTime("18:35"))
    }
  }
}
