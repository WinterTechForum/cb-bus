package crestedbutte

import com.billding.time.BusTime
import crestedbutte.routes.{RouteWithTimes, RtaSouthbound}
import utest.{TestSuite, Tests}
import utest._

object RoundTripCalculatorTest extends TestSuite {
  val tests = Tests {
    test("departure leg tests") {

      val arrivalTime: BusTime = BusTime("07:20")
      val destination: Location.Value = Location.BrushCreek
      val leaveSchedule: RouteWithTimes = RtaSouthbound.fullSchedule.routeWithTimes
      val results =
      RoundTripCalculator.findLatestDepartureLeg(arrivalTime, destination, leaveSchedule)
      assert( results.stops.head.busTime == BusTime("06:40"))
    }
  }
}
