package crestedbutte.dom

import animus.Animation
import com.raquo.laminar.api.L.*
import com.raquo.laminar.nodes.ReactiveHtmlElement
import crestedbutte.*
import crestedbutte.laminar.LocationTimeDirection

object BulmaLocal {
  def locationwithTime(
    l: LocationWithTime,
    segment: RouteSegment,
    updates: Sink[LocationTimeDirection],
  ) = {
    val buttonModifier =
      if (segment.start == l || segment.end == l)
        " is-primary"
      else
        ""
    div(
      textAlign := "center",
      paddingBottom := "3px",
      cls := "time",
      button(
        cls := s"clickable-time button m-2 $buttonModifier",
        onClick.mapTo {
          LocationTimeDirection(l, segment)
        } --> updates,
        l.t.toDumbAmericanString,
      ),
    )
  }

  def UpcomingStops(
    scheduleAtStop: BusScheduleAtStop,
    routeSegment: RouteSegment,
    selectedTimeUpdater: Sink[LocationTimeDirection],
  ) = {
    println("UPcoming stops: " + scheduleAtStop.location)
    val $opacity = Animation.from(0).wait(250).to(1).run
    val $width = Animation.from(0).wait(250).to(100).run
    div(
      width <-- $width.map(_.toString + "%"),
      opacity <-- $opacity,
      h4(textAlign := "center", scheduleAtStop.location.name),
      h5(textAlign := "center", "Upcoming Arrivals"),
      scheduleAtStop.locationsWithTimes.map { l =>
        locationwithTime(l, routeSegment, selectedTimeUpdater)
      },
    )
  }

}
