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
                      ) = {
    println("segment: " + segment)
    div(
      textAlign := "center",
      verticalAlign := "middle",
      paddingBottom := "3px",
      cls := "time",
      div(
        div(
          span(
            cls := "clickable-time",
            onClick.mapTo {
              {
                println("Clicked: " + l)
                LocationTimeDirection(l, segment)
              }
            } --> updates,
            l.t.toDumbAmericanString,
          )
        ),
      ),
    )
  }

  def UpcomingStops(
    scheduleAtStop: BusScheduleAtStop, // TODO This needs to be pairs.
    plan: Plan,
    routeSegment: RouteSegment,
    selectedTimeUpdater: Sink[LocationTimeDirection],
  ) = {
    println("UPcoming stops: " + scheduleAtStop.location)
    div(
      h4(textAlign := "center", scheduleAtStop.location.name),
      h5(textAlign := "center", "Upcoming Arrivals"),
      scheduleAtStop.locationsWithTimes.map(l => {
        locationwithTime(l, routeSegment, selectedTimeUpdater)
      }
      ),
    )
  }

  def manualClunkyAlerts(
    $alertsEnabled: Signal[Boolean],
    time: WallTime,
  ) =
    div(
      child <-- $alertsEnabled.map(alertsEnabled =>
        if (dom.Notification.permission == "granted" && alertsEnabled)
          Experimental.Notifications.AlarmIcon(
            "glyphicons-basic-443-bell-ringing.svg",
            "arrival-time-alarm",
            time,
          )
        else div(),
      ),
    )

}
