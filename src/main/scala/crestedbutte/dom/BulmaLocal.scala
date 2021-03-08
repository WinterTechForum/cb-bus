package crestedbutte.dom

import crestedbutte.BusScheduleAtStop
import com.raquo.laminar.api.L._
import crestedbutte.laminar.{Components, Experimental, TagsOnlyLocal}
import org.scalajs.dom.experimental.Notification

object BulmaLocal {

  def bulmaModal(
    scheduleAtStop: BusScheduleAtStop,
    $alertsEnabled: Signal[Boolean],
    $active: Var[Boolean],
  ) =
    div(
      cls := "modal",
      cls <-- $active.signal.map(
        active => if (active) "is-active" else "",
      ),
      div(cls := "modal-background"),
      div(
        cls := "modal-content",
        marginLeft := "45px",
        marginRight := "45px",
        div(
          h4(textAlign := "center", scheduleAtStop.location.name),
          h5(textAlign := "center", "Upcoming Arrivals"),
          scheduleAtStop.times.map(
            time =>
              div(
                textAlign := "center",
                verticalAlign := "middle",
                paddingBottom := "3px",
                span(time.toDumbAmericanString),
                child <-- $alertsEnabled.map(
                  alertsEnabled =>
                    if (Notification.permission == "granted" && alertsEnabled) // TODO Make this check less Stringy
                      Experimental.Notifications.AlarmIcon(
                        "glyphicons-basic-443-bell-ringing.svg",
                        "arrival-time-alarm",
                        time,
                      )
                    else div(),
                ),
              ),
          ),
        ),
      ),
      button(
        cls := "modal-close is-large",
        aria.label := "close",
        onClick.preventDefault.map(_ => {
          org.scalajs.dom.document
            .querySelector("html")
            .classList
            .remove("is-clipped")
          println("Clicked modal close")
          false
        }) --> $active,
      ),
    )

}
