package crestedbutte

import com.billding.time.WallTime
import crestedbutte.{RTA, RouteWithTimes}
import zio.test.*

object RouteValidityTest extends ZIOSpecDefault {
  val routeLeg1 =
    RouteLeg(
      Seq(
        LocationWithTime(Location.GunnisonLibrary, WallTime("07:29")),
        LocationWithTime(Location.Safeway, WallTime("07:34")),
      ),
      RTA.Northbound.componentName,
    ).getOrElse(???)

  def spec =
    suite("RouteValidityTest")(
      test("routeLeg1") {
        // TODO Test for this logic used in SwipeUpdater
        // routeSegment.routeWithTimes.nextAfter(routeSegment),
        assertCompletes
      },
    )
}
