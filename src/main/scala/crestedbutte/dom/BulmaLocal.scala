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
  def bulmaModal(
    scheduleAtStop: BusScheduleAtStop,
    $active: Var[Boolean],
    plan: Plan,
    selectedTimeUpdater: Sink[LocationTimeDirection],
    // TODO pass state piece is being updated
  ) = {
    val notificationBus = EventBus[ReactiveHtmlElement[_]]()
    div(
      cls := "modal",
      cls <-- $active.signal.map(active =>
        if (active) "is-active" else "",
      ),
      div(cls := "modal-background"),
      div(
        cls := "modal-content",
        marginLeft := "45px",
        marginRight := "45px",
        child <-- notificationBus.events.map(element => element),
        UpcomingStops(
          scheduleAtStop,
          plan,
          selectedTimeUpdater,
        ),
      ),
      button(
        cls := "modal-close is-large",
        aria.label := "close",
        onClick.preventDefault.map { _ =>
          org.scalajs.dom.document
            .querySelector("html")
            .classList
            .remove("is-clipped")
          false
        } --> $active,
      ),
    )
  }

  def locationwithTime(
    // pair with other end somehow
    l: LocationWithTime,
    plan: Plan, // TODO Instead of passing Plan, we should just emit an event with the new selected time.
    updates: Sink[LocationTimeDirection],
  ) =
    div(
      textAlign := "center",
      verticalAlign := "middle",
      paddingBottom := "3px",
      cls := "time",
      div(
        div(
          plan.l.map(segment =>
            span(
              cls := "clickable-time",
              onClick.mapTo {
                LocationTimeDirection(l, segment)
              } --> updates,
              l.t.toDumbAmericanString,
            ),
          ),
        ),
      ),
    )

  def UpcomingStops(
    scheduleAtStop: BusScheduleAtStop, // TODO This needs to be pairs.
    plan: Plan,
    selectedTimeUpdater: Sink[LocationTimeDirection],
    // TODO pass state piece is being updated
  ) = {
    div(
      h4(textAlign := "center", scheduleAtStop.location.name),
      h5(textAlign := "center", "Upcoming Arrivals"),
      scheduleAtStop.locationsWithTimes.map(l =>
        locationwithTime(l, plan, selectedTimeUpdater),
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
