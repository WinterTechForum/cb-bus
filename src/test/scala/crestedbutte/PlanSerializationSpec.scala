package crestedbutte

import com.billding.time.WallTime
import crestedbutte.routes.RtaSouthbound
import zio.json.*
import zio.test.*
import zio.*

object PlanSerializationSpec extends ZIOSpecDefault {
  val plan =
    Plan(
      Seq(
        RouteSegment(
          RtaSouthbound.componentName,
          LocationWithTime(Location.CBSouth, WallTime("10:20 PM")),
          LocationWithTime(Location.RecCenter, WallTime("10:46 PM")),
        ),
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
        },
        suite("url encoding")(
          test("is reasonable") {
            val result = UrlEncoding.encode(plan)
            val expected =
              "eyJsIjpbeyJyIjowLCJzIjp7ImwiOjI0LCJ0IjoxMzQwfSwiZSI6eyJsIjoyOCwidCI6MTM2Nn19XX0="
            assertTrue(result == expected)
          },
          test("round trips") {
            val input =
              "eyJsIjpbeyJyIjowLCJzIjp7ImwiOjI0LCJ0IjoxMzQwfSwiZSI6eyJsIjoyOCwidCI6MTM2Nn19XX0="

            assertTrue(
              UrlEncoding.decode(input).getOrElse(???) == plan,
            )
          },
        ),
      ),
    )

}
