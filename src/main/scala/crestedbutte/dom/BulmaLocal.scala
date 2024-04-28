package crestedbutte.dom

import com.billding.time.WallTime
import com.raquo.laminar.api.L.*
import com.raquo.laminar.nodes.ReactiveHtmlElement
import crestedbutte.*
import crestedbutte.laminar.Experimental
import org.scalajs.dom
import org.scalajs.dom.experimental.Notification

object BulmaLocal {
  enum ModalMode:
    case UpcomingStops
    case SelectedLeg(
      routeLeg: RouteLeg)

  def notification(
    text: String,
  ) =
    div(
      cls := "notification is-link is-light",
      button(cls := "delete"),
      text,
      a("TODO* Link to TripViewer *TODO*"),
    )

  def notificationWithHomeLink(
    text: String,
    componentSelector: Observer[ComponentData],
  ) =
    div(
      cls := "notification is-link is-light",
      button(cls := "delete"),
      text,
      button(
        cls := "button",
        "View Current Trip",
        onClick.mapTo(PlanViewer) --> componentSelector,
      ),
    )

  def bulmaModal(
    scheduleAtStop: BusScheduleAtStop,
    $active: Var[Boolean],
  ) =
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

  def manualClunkyAlerts(
                          $alertsEnabled: Signal[Boolean],
                          time: WallTime,
                        ) =
    div(
          child <-- $alertsEnabled.map(alertsEnabled =>
            if (
              dom.Notification.permission == "granted" && alertsEnabled
            )
              Experimental.Notifications.AlarmIcon(
                "glyphicons-basic-443-bell-ringing.svg",
                "arrival-time-alarm",
                time,
              )
            else div(),
          )
    )

  def UpcomingStops(
    scheduleAtStop: BusScheduleAtStop,
  ) =
    div(
      h4(textAlign := "center", scheduleAtStop.location.name),
      h5(textAlign := "center", "Upcoming Arrivals"),
      scheduleAtStop.times.map(time =>
        div(
          textAlign := "center",
          verticalAlign := "middle",
          paddingBottom := "3px",
          span(
            time.toDumbAmericanString,
          ),
        ),
      ),
    )

}
