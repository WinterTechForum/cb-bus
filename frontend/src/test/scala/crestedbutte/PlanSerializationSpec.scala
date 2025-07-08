package crestedbutte

import com.billding.time.WallTime
import crestedbutte.routes.RTA
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

  def spec =
    suite("PlanSerializationSpec")(
      suite("encoding")(
        test("is nice and compact") {
          val expected =
            """|{
              |  "l" : [
              |    {
              |      "r" : 0,
              |      "s" : {
              |        "l" : 24,
              |        "t" : 1340
              |      },
              |      "e" : {
              |        "l" : 28,
              |        "t" : 1366
              |      }
              |    }
              |  ]
              |}""".stripMargin
          assertTrue(plan.toJsonPretty == expected)
        } @@ TestAspect.ignore,
        suite("url encoding")(
          test("is reasonable") {
            val result = UrlEncoding.encode(plan)
            val expected =
              "eyJsIjpbeyJyIjowLCJzIjp7ImwiOjI0LCJ0IjoxMzQwfSwiZSI6eyJsIjoyOCwidCI6MTM2Nn19XX0="
            assertTrue(result == expected)
          } @@ TestAspect.ignore,
          test("round trips") {
            val input =
              "eyJsIjpbeyJyIjowLCJzIjp7ImwiOjI0LCJ0IjoxMzQwfSwiZSI6eyJsIjoyOCwidCI6MTM2Nn19XX0="

            assertTrue(
              UrlEncoding.decode(input).getOrElse(???) == plan,
            )
          } @@ TestAspect.ignore,
        ),
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
