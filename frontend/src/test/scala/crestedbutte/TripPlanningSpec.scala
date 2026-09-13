package crestedbutte

import com.billding.time.WallTime
import crestedbutte.laminar.StopPicker
import zio.test.*

object TripPlanningSpec extends ZIOSpecDefault {
  private def outbound = RTA.Southbound.fullSchedule
    .segment(Location.MountaineerSquare, Location.FourWayUphill).get.head

  def spec = suite("reuse trips and find stops")(
    test("reuse selects the next departure and preserves the ride identity") {
      val original = outbound
      val cutoff = WallTime("10:00")
      val updated = TripPlanning.useNow(Plan(Seq(original)), cutoff).toOption.get.l.head
      assertTrue(updated.start.t.isAfter(cutoff), updated.id == original.id,
        updated.start.l == original.start.l, updated.end.l == original.end.l,
        updated.route == original.route)
    },
    test("stale saved times are replaced using the current timetable") {
      val original = outbound
      val outdated = RouteSegment.attempt(original.route, original.start.copy(t = WallTime("05:01")),
        original.end.copy(t = WallTime("05:09"))).toOption.get
      val updated = TripPlanning.useNow(Plan(Seq(outdated)), WallTime("10:00")).toOption.get.l.head
      assertTrue(TripPlanning.departures(original).exists(s => s.start == updated.start && s.end == updated.end))
    },
    test("planned stopovers survive reuse") {
      val first = outbound
      val returnRide = RTA.Northbound.fullSchedule
        .segment(first.end.l, first.start.l).get.find(_.start.t.localTime.value > first.end.t.localTime.value + 45).get
      val oldPause = returnRide.start.t.localTime.value - first.end.t.localTime.value
      val updated = TripPlanning.useNow(Plan(Seq(first, returnRide)), WallTime("10:00")).toOption.get
      val newPause = updated.l(1).start.t.localTime.value - updated.l.head.end.t.localTime.value
      assertTrue(newPause >= oldPause, updated.l.map(_.id) == Seq(first.id, returnRide.id))
    },
    test("no remaining service fails without returning a partial trip") {
      val plan = Plan(Seq(outbound))
      assertTrue(TripPlanning.useNow(plan, WallTime("23:59")).isLeft)
    },
    test("overnight stopovers cannot silently wrap back to today's morning") {
      val first = outbound
      val overnight = RTA.Northbound.fullSchedule.segment(first.end.l, first.start.l).get.head
      val shifted = RouteSegment.fromRouteLeg(RouteLeg(Seq(
        overnight.start.copy(t = WallTime("04:00")), overnight.end), overnight.route).get)
      assertTrue(TripPlanning.useNow(Plan(Seq(first, shifted)), WallTime("10:00")).isLeft)
    },
    test("landmarks and towns work in case-insensitive stop search") {
      assertTrue(StopPicker.matches(Location.SpencerAndHighwayOneThirtyFive, " WALMART "),
        StopPicker.matches(Location.DenverAndHighwayOneThirtyFive, "city market"),
        StopPicker.matches(Location.CBSouth, "red mtn"),
        StopPicker.matches(Location.Safeway, "gunnison"),
        !StopPicker.matches(Location.MountaineerSquare, "walmart"))
    },
  )
}
