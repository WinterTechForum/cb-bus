package crestedbutte

import com.billding.time.MinuteDuration
import com.billding.time.WallTime
import zio.test.Assertion._
import zio.test.{test, _} // Enables Int.minutes syntax

/*
object BusTimeSpec
    extends DefaultRunnableSpec(
      suite("HelloWorldSpec")(
        test("bus is catchable") {
          val now = BusTime("07:10")
          val busTime = BusTime("07:15")
          assert(BusTime.catchableBus(now, busTime), equalTo(true))
        },
        test("bus is catchable even if it's PM") {
          val now = BusTime("11:55")
          val busTime = BusTime("12:05")
          assert(BusTime.catchableBus(now, busTime), equalTo(true))
        },
        test("duration between times is accurate") {
          val now = BusTime("11:55")
          val busTime = BusTime("12:05")
          println(
            "Minutes between: " + now.between(busTime).toMinutes
          )
          assert(now.between(busTime), equalTo(10.minutes))
        },
        suite("parsing")(
          test("parses morning time") {
            assert(BusTime.parseIdeal("02:05").get.toString,
                   equalTo("02:05"))
          },
          test("parses evening time") {
            assert(BusTime.parseIdeal("06:22").get.toString,
                   equalTo("06:22"))
          }
        )
      )
    ) {}


 */
