package crestedbutte

import com.billding.time.WallTime
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
                LocationWithTime(Location.BrushCreek, WallTime("07:40")),
                LocationWithTime(Location.CBSouth, WallTime("07:50")),
                LocationWithTime(Location.Almont, WallTime("08:00")),
              )),
            RouteLeg(
              Seq(
                LocationWithTime(Location.BrushCreek, WallTime("08:00")),
                LocationWithTime(Location.Almont, WallTime("08:18")),
              )),
            RouteLeg(
              Seq(
                LocationWithTime(Location.BrushCreek, WallTime("08:30")),
                LocationWithTime(Location.CBSouth, WallTime("08:40")),
                LocationWithTime(Location.Almont, WallTime("08:50")),
              ))
          )
        )
      )

    }
  }
}
