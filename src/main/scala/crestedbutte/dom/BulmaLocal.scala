package crestedbutte.dom

import crestedbutte.BusScheduleAtStop
import com.raquo.laminar.api.L._
import crestedbutte.laminar.TagsOnlyLocal
import org.scalajs.dom.experimental.Notification

object BulmaLocal {

  def bulmaModal(
    scheduleAtStop: BusScheduleAtStop,
    idValue: String,
    $alertsEnabled: Signal[Boolean],
  ) =
    div(
      idAttr := idValue,
      cls := "modal",
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
                      TagsOnlyLocal.svgIconForAlarm(
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
      button(cls := "modal-close is-large", aria.label := "close"),
    )

}
