package crestedbutte.dom

import crestedbutte.*
import com.raquo.laminar.api.L.*
import com.raquo.laminar.nodes.ReactiveHtmlElement
import crestedbutte.laminar.{Components, Experimental}
import crestedbutte.pwa.Persistence
import org.scalajs.dom
import org.scalajs.dom.IDBDatabase
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
    $alertsEnabled: Signal[Boolean],
    $active: Var[Boolean],
    $mode: Var[ModalMode],
    namedRoute: NamedRoute,
    db: Persistence,
    componentSelector: Observer[ComponentData],
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
              cls := "selected-leg-header",
              button(
                cls := "button",
                onClick.mapTo(ModalMode.UpcomingStops) --> $mode,
                "Back to Upcoming Arrivals",
              ),
              Components.RouteLegElementInteractive(
                routeLeg,
                db,
                $active,
                notificationBus.writer,
                componentSelector,
              ),
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
          textAlign := "center",
          verticalAlign := "middle",
          paddingBottom := "3px",
          button(
            cls := "button",
            onClick
              .mapTo(
                ModalMode.SelectedLeg(
                  namedRoute.routeWithTimes.legs
                    .find(leg =>
                      leg.stops.contains(
                        LocationWithTime(scheduleAtStop.location,
                                         time,
                        ),
                      ),
                    )
                    .flatMap(
                      _.trimToStartAt(
                        scheduleAtStop.location,
                      ).toOption,
                    )
                    .get, // Unsafe
                ),
              ) --> $mode,
            time.toDumbAmericanString,
          ),
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
