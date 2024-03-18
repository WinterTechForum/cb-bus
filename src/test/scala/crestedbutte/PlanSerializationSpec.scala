package crestedbutte

import com.billding.time.WallTime
import crestedbutte.routes.RtaSouthbound
import zio.json.*
import zio.test.*
import zio.*

object PlanSerializationSpec extends ZIOSpecDefault {
  def spec =
    suite("PlanSerializationSpec")(
      suite("encoding")(
        test("is nice and compact"){
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
        }

      )
    )


}
