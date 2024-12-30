package crestedbutte.dom

import com.raquo.laminar.api.L.*
import com.raquo.laminar.nodes.ReactiveHtmlElement
import crestedbutte.*
import crestedbutte.laminar.LocationTimeDirection

object BulmaLocal {
  def locationwithTime(
    l: LocationWithTime,
    segment: RouteSegment,
    updates: Sink[LocationTimeDirection],
  ) =
    div(
      textAlign := "center",
      verticalAlign := "middle",
      paddingBottom := "3px",
      if (segment.start == l || segment.end == l)
        backgroundColor := "LightGreen"
      else
        cls := "",
      cls := "time",
      div(
        div(
          span(
            cls := "clickable-time",
            onClick.mapTo {
              LocationTimeDirection(l, segment)
            } --> updates,
            l.t.toDumbAmericanString,
          ),
        ),
      ),
    )

  def UpcomingStops(
    scheduleAtStop: BusScheduleAtStop,
    routeSegment: RouteSegment,
    selectedTimeUpdater: Sink[LocationTimeDirection],
  ) = {
    println("UPcoming stops: " + scheduleAtStop.location)
    div(
      h4(textAlign := "center", scheduleAtStop.location.name),
      h5(textAlign := "center", "Upcoming Arrivals"),
      scheduleAtStop.locationsWithTimes.map { l =>
        locationwithTime(l, routeSegment, selectedTimeUpdater)
      },
    )
  }

}
