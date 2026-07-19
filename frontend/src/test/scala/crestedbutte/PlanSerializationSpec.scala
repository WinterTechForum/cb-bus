package crestedbutte

import com.billding.time.WallTime
import crestedbutte.RTA
import zio.json.*
import zio.test.*
import zio.*

object PlanSerializationSpec extends ZIOSpecDefault {
  val plan =
    Plan(
      Seq(
        RouteSegment
          .attempt(
            RTA.Southbound.componentName,
            LocationWithTime(Location.CBSouth, WallTime("10:20 PM")),
            LocationWithTime(Location.RecCenter, WallTime("10:46 PM")),
          )
          .getOrElse(???),
      ),
    )

  // Compare two plans on their logical content, ignoring the ephemeral,
  // UI-only `id` that RouteSegment carries (case-class equality includes it,
  // but it is regenerated per session and is not part of the persisted plan).
  private def sameLegs(a: Plan, b: Plan): Boolean =
    a.l.map(s => (s.route, s.start, s.end)) ==
      b.l.map(s => (s.route, s.start, s.end))

  def spec =
    suite("PlanSerializationSpec")(
      // These guard the load paths that were silently dropping users to a
      // blank schedule: localStorage on reload, and the `?plan=` share URL.
      suite("round trips")(
        test("localStorage: toJson then fromJson preserves the plan") {
          assertTrue(plan.toJson.fromJson[Plan] == Right(plan))
        },
        test("localStorage Option[Plan] path preserves the plan") {
          val opt: Option[Plan] = Some(plan)
          assertTrue(opt.toJson.fromJson[Option[Plan]] == Right(opt))
        },
        test("URL: encode then decodePlan preserves the plan") {
          assertTrue(
            UrlEncoding.decodePlan(UrlEncoding.encode(plan)) == Right(plan),
          )
        },
      ),
      // Regression: adding the `id` field to RouteSegment made it a REQUIRED
      // key in the derived codec, so any plan JSON without it (older saves,
      // shared URLs, anything not produced by the exact current encoder)
      // failed to decode and the user lost their whole schedule on reload.
      // `id` is ephemeral UI state and must be optional on the wire.
      suite("backward compatibility")(
        test("decodes a plan whose segment omits the id field") {
          val legacyJson =
            """{"l":[{"r":0,"s":{"l":14,"t":1340},"e":{"l":18,"t":1366}}]}"""
          val decoded = legacyJson.fromJson[Plan]
          assertTrue(
            decoded.isRight,
            decoded.exists(sameLegs(_, plan)),
          )
        },
        test("decodes a legacy plan embedded in a share URL") {
          val legacyJson =
            """{"l":[{"r":0,"s":{"l":14,"t":1340},"e":{"l":18,"t":1366}}]}"""
          val legacyUrl =
            java.util.Base64.getUrlEncoder
              .encodeToString(legacyJson.getBytes())
          val decoded = UrlEncoding.decodePlan(legacyUrl)
          assertTrue(
            decoded.isRight,
            decoded.exists(sameLegs(_, plan)),
          )
        },
      ),
      suite("plain text")(
        test("single leg") {
          assertTrue(
            plan.plainTextRepresentation ==
              """10:20 PM  CB South
                |10:46 PM  Rec Center
                |""".stripMargin,
          )
        },
        test("multi leg") {
          assertTrue(
            plan
              .copy(
                plan.l :+
                  RouteSegment
                    .attempt(
                      RTA.Southbound.componentName,
                      LocationWithTime(
                        Location.SpencerAndHighwayOneThirtyFive,
                        WallTime("03:20 PM"),
                      ),
                      LocationWithTime(Location.BrushCreek,
                                       WallTime("04:00 PM"),
                      ),
                    )
                    .getOrElse(???),
              )
              .plainTextRepresentation ==
              """10:20 PM  CB South
                |10:46 PM  Rec Center
                |
                |03:20 PM  Spencer & Highway 135
                |04:00 PM  Brush Creek
                |""".stripMargin,
          )
        },
      ),
    )

}
