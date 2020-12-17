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
      assert( results.stops.head.busTime == BusTime("07:10"))
    }
  }
}
