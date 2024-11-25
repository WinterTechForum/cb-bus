package crestedbutte

import com.raquo.airstream.state.Var
import zio.test._

object PlanSpec extends ZIOSpecDefault {
  def spec =
    suite("PlanSpec")(
      suite("select new time from list") (
        test("happy path") {

          val scheduleAtStop: BusScheduleAtStop = ???
          val $plan: Var[Plan] = Var(Plan(Seq.empty))

          assertCompletes
        }
      )
    )

}
