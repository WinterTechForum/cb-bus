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
      )
    )

  def spec =
    suite("PlanSerializationSpec")(
      suite("encoding")(
        test("is nice and compact"){
          println(plan.toJsonPretty)
          val expected =
            """|{
              |  "legs" : [
              |    {
              |      "route" : "Rta Southbound",
              |      "start" : {
              |        "location" : "CB South",
              |        "t" : "10:20 PM"
              |      },
              |      "end" : {
              |        "location" : "Rec Center",
              |        "t" : "10:46 PM"
              |      }
              |    }
              |  ]
              |}""".stripMargin
          assertTrue(plan.toJsonPretty ==  expected)
        },
        test("final url encoding is reasonable") {
          val result = UrlEncoding.encode(plan)
          val expected =
            "%7B%22legs%22%3A%5B%7B%22route%22%3A%22Rta+Southbound%22%2C%22start%22%3A%7B%22location%22%3A%22CB+South%22%2C%22t%22%3A%2210%3A20+PM%22%7D%2C%22end%22%3A%7B%22location%22%3A%22Rec+Center%22%2C%22t%22%3A%2210%3A46+PM%22%7D%7D%5D%7D"
            "%7B%22legs%22%3A%5B%7B%22route%22%3A%22Rta+Southbound%22%2C%22start%22%3A%7B%22location%22%3A%22CB+South%22%2C%22t%22%3A1340%7D%2C%22end%22%3A%7B%22location%22%3A%22Rec+Center%22%2C%22t%22%3A1366%7D%7D%5D%7D"
            "%7B%22legs%22%3A%5B%7B%22route%22%3A%22Rta+Southbound%22%2C%22start%22%3A%7B%22location%22%3A24%2C%22t%22%3A1340%7D%2C%22end%22%3A%7B%22location%22%3A28%2C%22t%22%3A1366%7D%7D%5D%7D"
            "eyJsZWdzIjpbeyJyb3V0ZSI6IlJ0YSBTb3V0aGJvdW5kIiwic3RhcnQiOnsibG9jYXRpb24iOjI0LCJ0IjoxMzQwfSwiZW5kIjp7ImxvY2F0aW9uIjoyOCwidCI6MTM2Nn19XX0="
          assertTrue(result == expected)

        }

      )
    )


}
