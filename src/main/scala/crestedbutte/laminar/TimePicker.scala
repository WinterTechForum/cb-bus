package crestedbutte.laminar

import com.billding.time.BusTime
import com.raquo.laminar.api.L._

object TimePicker {

  sealed trait AM_OR_PM
  case object AM extends AM_OR_PM
  case object PM extends AM_OR_PM

  def Toggler(
    $value: Var[AM_OR_PM],
  ) = {
    val updates = new EventBus[Unit]
    val newNumberValues: EventStream[AM_OR_PM] =
      updates.events.withCurrentValueOf($value).map {
        case (curNumberValue) =>
          if (curNumberValue == AM)
            PM
          else
            AM

      }

    div(
      cls := "amOrPm wheel",
      button(
        cls := "arrival-time adjuster-button open-arrival-time-modal tp-inc",
        onClick.preventDefault.map(_ => ()) --> updates,
        img(
          cls := "glyphicon",
          src := "/glyphicons/svg/individual-svg/glyphicons-basic-222-chevron-up.svg",
        ),
      ),
      div(
        cls := "tp-display",
        child <-- $value.signal.map(_.toString),
      ),
      button(
        cls := "arrival-time adjuster-button open-arrival-time-modal tp-dec",
        onClick.preventDefault.map(_ => ()) --> updates,
        img(
          cls := "glyphicon",
          src := "/glyphicons/svg/individual-svg/glyphicons-basic-221-chevron-down.svg",
        ),
      ),
      newNumberValues --> $value,
    )

  }

  def NumberPicker(
    $number: Var[Int],
    deltaValue: Int,
    minValue: Int,
    maxValue: Int,
    sectionName: String,
  ) = {
    val updates = new EventBus[Int]
    val newNumberValues: EventStream[Int] =
      updates.events.withCurrentValueOf($number).map {
        case (delta, curNumberValue) =>
          if (delta > 0)
            if (curNumberValue + delta <= maxValue)
              curNumberValue + delta
            else
              curNumberValue
          else if (curNumberValue + delta >= minValue)
            curNumberValue + delta
          else
            curNumberValue

      }

    div(
      cls := s"$sectionName wheel",
      button(
        cls := s"arrival-time adjuster-button open-arrival-time-modal tp-inc",
        onClick.preventDefault.map(_ => deltaValue) --> updates,
        img(
          cls := "glyphicon",
          src := "/glyphicons/svg/individual-svg/glyphicons-basic-222-chevron-up.svg",
        ),
      ),
      div(
        cls := s"tp-display",
        child <-- $number.signal.map(_.toString),
      ),
      button(
        cls := s"arrival-time adjuster-button open-arrival-time-modal tp-dec",
        onClick.preventDefault.map(_ => -deltaValue) --> updates,
        img(
          cls := "glyphicon",
          src := "/glyphicons/svg/individual-svg/glyphicons-basic-221-chevron-down.svg",
        ),
      ),
      newNumberValues --> $number,
    )

  }

  def TimePicker() = {

    val $hours = Var(1)
    val $minutes = Var(0)
    val $amOrPm = Var(AM: AM_OR_PM)
    val initialTime = BusTime("08:00")

    val fullTime: Signal[BusTime] =
      Signal
        .combine($hours, $minutes, $amOrPm)
        .foldLeft(_ => initialTime) {
          // TODO Leading zero formating. Use BusTime class if possible
          case (_, (hours, minutes, amOrPm)) =>
            try {
              BusTime(s"$hours:$minutes")
            } catch {
              case e: Exception =>
                println("busTime parse exception: " + e.getMessage)

                BusTime("10:00")
            }
        }

    (fullTime,
     div(
       cls := "time-picker-simple",
       child <-- Signal.combine($hours, $minutes, $amOrPm).map {
         // TODO Leading zero formating. Use BusTime class if possible
         case (hours, minutes, amOrPm) => s"$hours:$minutes $amOrPm"
       },
       NumberPicker($hours, 1, 1, 12, "hour"),
       NumberPicker($minutes, 10, 0, 59, "minute"),
       Toggler($amOrPm),
     ))
  }

}
