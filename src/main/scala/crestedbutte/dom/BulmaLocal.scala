package crestedbutte.dom

import com.billding.time.WallTime
import com.raquo.laminar.api.L.*
import com.raquo.laminar.nodes.ReactiveHtmlElement
import crestedbutte.*
import crestedbutte.laminar.{
  Experimental,
  LocationTimeDirection,
  SelectedSegmentPiece,
}
import org.scalajs.dom
import org.scalajs.dom.experimental.Notification

object BulmaLocal {
  def locationwithTime(
    // pair with other end somehow
    l: LocationWithTime,
    // TODO pass in segment, instead of plan?
    segment: RouteSegment, // TODO Instead of passing Plan, we should just emit an event with the new selected time.
    updates: Sink[LocationTimeDirection],
  ) =
    div(
      textAlign := "center",
      verticalAlign := "middle",
      paddingBottom := "3px",
      (if (segment.start == l || segment.end == l)
        backgroundColor := "LightGreen"
      else
        cls := ""),
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
    scheduleAtStop: BusScheduleAtStop, // TODO This needs to be pairs.
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
