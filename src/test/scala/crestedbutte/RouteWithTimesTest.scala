package crestedbutte

import com.billding.time.WallTime
import crestedbutte.routes.{RouteWithTimes, RtaSouthbound}
import zio.test.*

object RouteWithTimesTest extends ZIOSpecDefault {
  val routeLeg1 =
    RouteLeg(
      Seq(
        LocationWithTime(Location.BrushCreek, WallTime("07:40")),
        LocationWithTime(Location.CBSouth, WallTime("07:50")),
        LocationWithTime(Location.Almont, WallTime("08:00")),
      ),
      RtaSouthbound.componentName)
    
  val routeLeg2 =RouteLeg(
    Seq(
      LocationWithTime(Location.BrushCreek, WallTime("08:00")),
      LocationWithTime(Location.CBSouth, WallTime("08:10")),
      LocationWithTime(Location.Almont, WallTime("08:20")),
    ), RtaSouthbound.componentName
  )
  
  val routeLeg3 = RouteLeg(
    Seq(
      LocationWithTime(Location.BrushCreek, WallTime("08:30")),
      LocationWithTime(Location.CBSouth, WallTime("08:40")),
      LocationWithTime(Location.Almont, WallTime("08:50")),
    ), RtaSouthbound.componentName)
    
  val routeWithTimes =
    RouteWithTimes(
      Seq(
        routeLeg1,
        routeLeg2,
        routeLeg3
      )
    )

  val spec = {
    suite("RouteWithTimes")(
      suite("nextBefore") (
        test("nextBefore(routeLeg1.ends) gives None"):
          assertTrue:
            routeWithTimes.nextBefore:
              routeLeg1.ends
            .isEmpty
        ,
        test("nextBefore(routeLeg2.ends) gives routeLeg1.ends"):
          assertTrue:
            routeWithTimes.nextBefore:
              routeLeg2.ends
            .contains:
              routeLeg1.ends
        ,
        test("nextBefore(routeLeg3.ends) gives routeLeg2.ends"):
          assertTrue:
            routeWithTimes.nextBefore:
              routeLeg3.ends
            .contains:
              routeLeg2.ends
      ),
      suite("nextAfter") (
        test("nextAfter(routeLeg1.ends) gives routeLeg2.ends"):
          assertTrue:
            routeWithTimes.nextAfter:
              routeLeg1.ends
            .contains:
              routeLeg2.ends
        ,
        test("nextAfter(routeLeg2.ends) gives routeLeg3.ends"):
          assertTrue:
            routeWithTimes.nextAfter:
              routeLeg2.ends
            .contains:
              routeLeg3.ends
        ,
        test("nextAfter(routeLeg3.ends) gives None"):
          assertTrue:
            routeWithTimes.nextAfter:
              routeLeg3.ends
            .isEmpty
      )
    )
  }
}
