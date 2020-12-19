package crestedbutte

import com.billding.time.BusTime
import crestedbutte.routes.{RouteWithTimes, RtaSouthbound}
import utest.{TestSuite, Tests}
import utest._

object RouteWithTimesTest extends TestSuite {

  val tests = Tests {
    test("routeLeg tests") {
//      pprint.pprintln(
//      RtaSouthbound.fullSchedule.routeWithTimes
//      )

//            pprint.pprintln(
//            RtaSouthbound.fullSchedule
//            )

//      pprint.pprintln(
//        RtaSouthbound.normalRouteWithTimes.routeLeg(1)
//      )

//      pprint.pprintln(
//        RtaSouthbound.expressRouteWithTimes.routeLeg(1)
//      )

      pprint.pprintln(
        RouteWithTimes(
          Seq(
            RouteLeg(
              Seq(
                LocationWithTime(Location.BrushCreek, BusTime("07:40")),
                LocationWithTime(Location.CBSouth, BusTime("07:50")),
                LocationWithTime(Location.Almont, BusTime("08:00")),
              )),
            RouteLeg(
              Seq(
                LocationWithTime(Location.BrushCreek, BusTime("08:00")),
                LocationWithTime(Location.Almont, BusTime("08:18")),
              )),
            RouteLeg(
              Seq(
                LocationWithTime(Location.BrushCreek, BusTime("08:30")),
                LocationWithTime(Location.CBSouth, BusTime("08:40")),
                LocationWithTime(Location.Almont, BusTime("08:50")),
              ))
          )
        )
      )

    }
  }
}
