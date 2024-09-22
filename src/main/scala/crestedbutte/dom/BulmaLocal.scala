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

  def bulmaModal(
    scheduleAtStop: BusScheduleAtStop,
    $active: Var[Boolean],
    $plan: Var[Plan],
    // TODO pass state piece is being updated
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
          $plan
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

  def locationwithTime(
    l: LocationWithTime,
    $plan: Var[Plan],
                      ) =
    div(
      textAlign := "center",
      verticalAlign := "middle",
      paddingBottom := "3px",
      cls := "time",

      div(
        child <-- $plan.signal.map(
          plan =>
            div(
            plan.l.map(
            segment =>
              span(
                cls:="clickable-time",
                onClick --> Observer {
                  _ =>
                    // TODO We need to track the previous state, so we know which one to update here.
                    $plan.update(plan => plan.copy(l = plan.l.map {
                      lwt => lwt.updateTimeAtLocation(l, segment.start.t, segment.end.t)
                    }))
                    println("$plan maybe.1: " + $plan.now())
                },
                l.t.toDumbAmericanString,

            )

          )
        )
        )

      ),
    )


  def UpcomingStops(
    scheduleAtStop: BusScheduleAtStop,
    $plan: Var[Plan],
    // TODO pass state piece is being updated
  ) =
    div(
      h4(textAlign := "center", scheduleAtStop.location.name),
      h5(textAlign := "center", "Upcoming Arrivals"),
      scheduleAtStop.locationsWithTimes.map(
        l => locationwithTime(l, $plan)
      ),
    )

  def notification(
                    text: String,
                  ) =
    div(
      cls := "notification is-link is-light",
      button(cls := "delete"),
      text,
      a("TODO* Link to TripViewer *TODO*"),
    )

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
