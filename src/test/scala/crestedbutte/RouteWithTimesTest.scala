package crestedbutte

import com.billding.time.BusTime
import crestedbutte.routes.{RouteWithTimes, RtaSouthbound}
import utest.{TestSuite, Tests}
import utest._

object RouteWithTimesTest extends TestSuite {

  val tests = Tests {
    test("routeLeg tests") {
      pprint.pprintln(
      RtaSouthbound.fullSchedule.routeWithTimes
      )

            pprint.pprintln(
            RtaSouthbound.fullSchedule
            )

//      pprint.pprintln(
//        RtaSouthbound.normalRouteWithTimes.routeLeg(1)
//      )

//      pprint.pprintln(
//        RtaSouthbound.expressRouteWithTimes.routeLeg(1)
//      )


    }
  }
}
