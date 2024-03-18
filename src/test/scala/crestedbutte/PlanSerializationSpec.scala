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
          println(plan.toJsonPretty)
          val expected =
            """|{
              |  "legs" : [
              |    {
              |      "route" : "Rta Southbound",
              |      "start" : {
              |        "location" : 24,
              |        "t" : 1340
              |      },
              |      "end" : {
              |        "location" : 28,
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
              "eyJsZWdzIjpbeyJyb3V0ZSI6IlJ0YSBTb3V0aGJvdW5kIiwic3RhcnQiOnsibG9jYXRpb24iOjI0LCJ0IjoxMzQwfSwiZW5kIjp7ImxvY2F0aW9uIjoyOCwidCI6MTM2Nn19XX0="
            assertTrue(result == expected)
          },
          test("round trips") {
            val input =
              "eyJsZWdzIjpbeyJyb3V0ZSI6IlJ0YSBTb3V0aGJvdW5kIiwic3RhcnQiOnsibG9jYXRpb24iOjI0LCJ0IjoxMzQwfSwiZW5kIjp7ImxvY2F0aW9uIjoyOCwidCI6MTM2Nn19XX0="

            assertTrue(
              UrlEncoding.decode(input).getOrElse(???) == plan,
            )
          },
        ),
      ),
    )

}
