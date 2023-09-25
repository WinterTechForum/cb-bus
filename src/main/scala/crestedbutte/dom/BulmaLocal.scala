package crestedbutte.dom

import crestedbutte.*
import com.raquo.laminar.api.L.*
import crestedbutte.laminar.{Components, Experimental, TagsOnlyLocal}
import org.scalajs.dom
import org.scalajs.dom.experimental.Notification

object BulmaLocal {
  enum ModalMode:
    case UpcomingStops
    case SelectedLeg(
      routeLeg: RouteLeg)

  def bulmaModal(
    scheduleAtStop: BusScheduleAtStop,
    $alertsEnabled: Signal[Boolean],
    $active: Var[Boolean],
    $mode: Var[ModalMode],
    namedRoute: NamedRoute,
  ) =
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
        child <-- $mode.signal.map:
          case ModalMode.UpcomingStops =>
            UpcomingStops(
              scheduleAtStop,
              $alertsEnabled,
              $mode,
              namedRoute,
            )
          case ModalMode.SelectedLeg(routeLeg) =>
            div(
              button(
                cls := "button",
                onClick.mapTo(ModalMode.UpcomingStops) --> $mode,
                "Back",
              ),
              // TODO Provide a way to go back to previous mode
              TagsOnlyLocal.RouteLeg("Route", routeLeg),
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
          println("Clicked modal close")
          false
        } --> $active,
      ),
    )

  def UpcomingStops(
    scheduleAtStop: BusScheduleAtStop,
    $alertsEnabled: Signal[Boolean],
    $mode: Var[ModalMode],
    namedRoute: NamedRoute,
  ) =
    div(
      h4(textAlign := "center", scheduleAtStop.location.name),
      h5(textAlign := "center", "Upcoming Arrivals"),
      scheduleAtStop.times.map(time =>
        div(
          onClick
            .mapTo(
              ModalMode.SelectedLeg(
                namedRoute.routeWithTimes.legs
                  .find(leg =>
                    leg.stops.contains(
                      LocationWithTime(scheduleAtStop.location, time),
                    ),
                  )
                  .map(_.trimToStartAt(scheduleAtStop.location))
                  .get, // Unsafe
              ),
            ) --> $mode,
          textAlign := "center",
          verticalAlign := "middle",
          paddingBottom := "3px",
          span(time.toDumbAmericanString),
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
          ),
        ),
      ),
    )

}
