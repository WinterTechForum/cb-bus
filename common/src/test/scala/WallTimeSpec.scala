import com.billding.time.WallTime
import zio.test.{assertTrue, suite, test, ZIOSpecDefault}

object WallTimeSpec extends ZIOSpecDefault {
  def spec =
    suite("WallTimeSpec")(
      test("parses 07:10 into 07:10") {
        val wt = WallTime("07:10")
        assertTrue(wt.toEUString == "07:10")
      },
      test("parses 12:05 PM into 12:05 PM dumb string") {
        val wt = WallTime("12:05 PM")
        assertTrue(wt.toDumbAmericanString == "12:05 PM")
      },
    )
}
