package crestedbutte.laminar

import crestedbutte.*
import crestedbutte.RTA
import zio.test.*

object ComponentsSpec extends ZIOSpecDefault {
  def spec =
    suite("Components return trip endpoints")(
      test(
        "southbound return trips leaving Rec Center start from Spencer",
      ) {
        val segment =
          RTA.Southbound.fullSchedule
            .segment(Location.MountaineerSquare, Location.RecCenter)
            .flatMap(_.headOption)
            .getOrElse(???)

        val (start, end) =
          Components.returnTripEndpoints(segment)

        assertTrue(
          start == Location.SpencerAndHighwayOneThirtyFive,
          end == Location.MountaineerSquare,
        )
      },
      test(
        "northbound return trips that began at Spencer end at Rec Center",
      ) {
        val segment =
          RTA.Northbound.fullSchedule
            .segment(Location.SpencerAndHighwayOneThirtyFive,
                     Location.MountaineerSquare,
            )
            .flatMap(_.headOption)
            .getOrElse(???)

        val (start, end) =
          Components.returnTripEndpoints(segment)

        assertTrue(
          start == Location.MountaineerSquare,
          end == Location.RecCenter,
        )
      },
      test(
        "rightLegOnRightRoute falls back to same direction when opposite misses",
      ) {
        val northLeg =
          RTA.Northbound.fullSchedule
            .segment(Location.Safeway,
                     Location.SpencerAndHighwayOneThirtyFive,
            )
            .flatMap(_.headOption)
            .getOrElse(???)
        val plan = Plan(Seq(northLeg))

        val maybeSegment =
          Components
            .rightLegOnRightRoute(
              Location.FourWayUphill,
              Location.MountaineerSquare,
              plan,
              northLeg.end.t,
            )

        assertTrue(
          maybeSegment.exists(_.route == RTA.Northbound.componentName),
        )
      },
    )
}
