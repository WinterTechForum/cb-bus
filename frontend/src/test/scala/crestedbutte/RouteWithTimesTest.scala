package crestedbutte

import com.billding.time.WallTime
import crestedbutte.routes.{RTA, RouteWithTimes}
import zio.test.*

object RouteWithTimesTest extends ZIOSpecDefault {
  val routeLeg1 =
    RouteLeg(
      Seq(
        LocationWithTime(Location.GunnisonLibrary, WallTime("07:29")),
        LocationWithTime(Location.Safeway, WallTime("07:34")),
      ),
      RTA.Northbound.componentName,
    ).getOrElse(???)

  val routeLeg2 = RouteLeg(
    Seq(
      LocationWithTime(Location.GunnisonLibrary, WallTime("07:59")),
      LocationWithTime(Location.Safeway, WallTime("08:04")),
    ),
    RTA.Northbound.componentName,
  ).getOrElse(???)

  val routeLeg3 = RouteLeg(
    Seq(
      LocationWithTime(Location.GunnisonLibrary, WallTime("08:29")),
      LocationWithTime(Location.Safeway, WallTime("08:34")),
    ),
    RTA.Northbound.componentName,
  ).getOrElse(???)

  val routeWithTimes =
    RouteWithTimes(
      Seq(
        routeLeg1,
        routeLeg2,
        routeLeg3,
      ),
    )

  val spec =
    suite("RouteWithTimes")(
      suite("nextBefore")(
        test("nextBefore(routeLeg1.ends) gives None"):
          assertTrue:
            routeWithTimes
              .nextBefore:
                RouteSegment.fromRouteLeg(
                  routeLeg1.ends.getOrElse(???),
                )
              .isEmpty
        ,
        test("nextBefore(routeLeg2.ends) gives routeLeg1.ends"):
          assertTrue:
            routeWithTimes
              .nextBefore:
                RouteSegment.fromRouteLeg(
                  routeLeg2.ends.getOrElse(???),
                )
              .contains:
                routeLeg1.ends
        ,
        test("nextBefore(routeLeg3.ends) gives routeLeg2.ends"):
          assertTrue:
            routeWithTimes
              .nextBefore:
                RouteSegment.fromRouteLeg(
                  routeLeg3.ends.getOrElse(???),
                )
              .contains:
                routeLeg2.ends.getOrElse(???),
      ),
      suite("nextAfter")(
        test("nextAfter(routeLeg1.ends) gives routeLeg2.ends"):
          assertTrue:
            routeWithTimes
              .nextAfter:
                RouteSegment.fromRouteLeg(
                  routeLeg1.ends.getOrElse(???),
                )
              .contains:
                routeLeg2.ends
        ,
        test("nextAfter(roueLeg2.ends) gives routeLeg3.ends")(
          assertTrue:
            routeWithTimes
              .nextAfter:
                RouteSegment.fromRouteLeg(
                  routeLeg2.ends.getOrElse(???),
                )
              .contains:
                routeLeg3.ends,
        ) @@ TestAspect.ignore,
        test("nextAfter(routeLeg3.ends) gives None"):
          assertTrue:
            routeWithTimes
              .nextAfter:
                RouteSegment.fromRouteLeg(
                  routeLeg3.ends.getOrElse(???),
                )
              .isEmpty,
      ),
    ) @@ TestAspect.ignore
  // http://localhost:8000/index_dev.html?plan=%7B%22legs%22%3A%5B%7B%22route%22%3A%22Rta+Southbound%22%2C%22start%22%3A%7B%22location%22%3A%22Riverbend%22%2C%22t%22%3A%2210%3A10+PM%22%7D%2C%22end%22%3A%7B%22location%22%3A%22CB+South%22%2C%22t%22%3A%2210%3A20+PM%22%7D%7D%2C%7B%22route%22%3A%22Rta+Southbound%22%2C%22start%22%3A%7B%22location%22%3A%224-way%22%2C%22t%22%3A%2210%3A38+PM%22%7D%2C%22end%22%3A%7B%22location%22%3A%22TallTexan%22%2C%22t%22%3A%2211%3A13+PM%22%7D%7D%5D%7D
}
