import com.billding.time.BusTime
import crestedbutte.BusSchedule
import zio.console._
import zio.test.Assertion._
import zio.test.environment._
import zio.test.{DefaultRunnableSpec, test, testM, _}

object BusTimeCalculationsSpec
    extends DefaultRunnableSpec(
      suite("HelloWorldSpec")(
        testM("sayHello correctly displays output") {
          for {
            _      <- putStrLn("Hello, World!")
            output <- TestConsole.output
          } yield assert(output, equalTo(Vector("Hello, World!\n")))
        },
        test(
          "stronger type"
        ) {
          assert(BusSchedule(
                   "09:00",
                   "09:15"
                 ).nextBusArrivalTime(BusTime("09:02")),
                 isSome(equalTo(BusTime("09:15"))))

        },
        test(
          "find the next bus time while in the middle of the scehdule"
        ) {
          assert(
            BusSchedule(
              "09:00",
              "09:15"
            ).nextBusArrivalTime(BusTime("09:10")),
            isSome(equalTo(BusTime("09:15")))
          )
        },
        test("really early morning check") {
          assert(
            BusSchedule(
              "07:10"
            ).nextBusArrivalTime(BusTime("05:00")),
            isSome(equalTo(BusTime("07:10")))
          )
        },
        test("after last bus has run") {
          assert(BusSchedule(
                   "23:40"
                 ).nextBusArrivalTime(BusTime("23:50")),
                 equalTo(Option.empty))
        },
        test("bus is arriving this minute") {
          assert(
            BusSchedule(
              "23:40"
            ).nextBusArrivalTime(BusTime("23:40")),
            isSome(equalTo(BusTime("23:40")))
          )
        }
      )
    ) {}
