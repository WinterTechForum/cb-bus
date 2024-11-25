package crestedbutte

import com.raquo.airstream.state.Var
import crestedbutte.routes.RtaNorthbound
import zio.test.*

object PlanSpec extends ZIOSpecDefault {
  def spec =
    suite("PlanSpec")(
      suite("select new time from list") (
        test("happy path") {

          val scheduleAtStop: BusScheduleAtStop = ???
          RtaNorthbound.fullSchedule.routeWithTimes
          val $plan: Var[Plan] = Var(Plan(Seq.empty))

          assertCompletes
        }
      )
    )

}
