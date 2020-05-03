package crestedbutte.dom

import crestedbutte.BusScheduleAtStop

object BulmaLocal {
  import scalatags.JsDom.all._

  def bulmaModal(scheduleAtStop: BusScheduleAtStop, idValue: String) =
    Bulma.modal(
      div(
        h4(textAlign := "center")(
          scheduleAtStop.location.name,
        ),
        h5(textAlign := "center")(
          "Upcoming Arrivals",
        ),
        scheduleAtStop.times.map(
          time =>
            div(textAlign := "center",
                verticalAlign := "middle",
                paddingBottom := "3px")(
              span(time.toDumbAmericanString),
              // TODO Re-enable once Notifications are more solid
              //              svgIconForAlarm(
              //                "glyphicons-basic-443-bell-ringing.svg",
              //                "arrival-time-alarm",
              //                time
              //              )
            ),
        ),
      ),
      idValue,
    )

}
