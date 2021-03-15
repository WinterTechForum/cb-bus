package crestedbutte.laminar

import com.billding.time.BusTime
import com.raquo.laminar.api.L._
import com.raquo.laminar.nodes.ReactiveHtmlElement
import org.scalajs.dom.html

object TimePicker {

  sealed trait AM_OR_PM
  case object AM extends AM_OR_PM
  case object PM extends AM_OR_PM

  def Toggler(
    initialValue: AM_OR_PM,
  ): (ReactiveHtmlElement[html.Div], StrictSignal[AM_OR_PM]) = {
    val updates = new EventBus[Unit]
    val $value: Var[AM_OR_PM] = Var(initialValue)
    val newNumberValues: EventStream[AM_OR_PM] =
      updates.events.withCurrentValueOf($value).map {
        case (curNumberValue) =>
          if (curNumberValue == AM)
            PM
          else
            AM

      }

    (
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
      ),
      $value.signal,
    )

  }

  def NumberPicker(
    initialValue: Int,
    deltaValue: Int,
    minValue: Int,
    maxValue: Int,
    sectionName: String,
  ) = {
    val $numberX: Var[Int] = Var(initialValue)

    val updates = new EventBus[Int]
    val newNumberValues: EventStream[Int] =
      updates.events.withCurrentValueOf($numberX).map {
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

    (
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
          child <-- $numberX.signal.map(_.toString),
        ),
        button(
          cls := s"arrival-time adjuster-button open-arrival-time-modal tp-dec",
          onClick.preventDefault.map(_ => -deltaValue) --> updates,
          img(
            cls := "glyphicon",
            src := "/glyphicons/svg/individual-svg/glyphicons-basic-221-chevron-down.svg",
          ),
        ),
        newNumberValues --> $numberX,
      ),
      $numberX.signal,
    )

  }

  def TimePicker() = {

    val (hourPicker, hourS) =
      NumberPicker(initialValue = 1,
                   deltaValue = 1,
                   minValue = 1,
                   maxValue = 12,
                   sectionName = "hour")
    val (minutePicker, minuteS) =
      NumberPicker(initialValue = 30,
                   deltaValue = 10,
                   minValue = 0,
                   maxValue = 59,
                   sectionName = "minute")
    val (amPmToggler, amOrPm) = Toggler(AM)
    val initialTime = BusTime("08:00")

    val fullTime: Signal[BusTime] =
      Signal
        .combine(hourS, minuteS, amOrPm)
        .foldLeft(_ => initialTime) {
          case (_, (hours, minutes, amOrPmInner)) => // TODO Use amOrPm
            try {
              val offset =
                if (amOrPmInner == AM)
                  0
                else
                  12

              BusTime(s"${hours + offset}:$minutes")
            } catch {
              case e: Exception =>
                println("busTime parse exception: " + e.getMessage)

                BusTime("10:00")
            }
        }

    (fullTime,
     div(
       cls := "time-picker-simple",
       child <-- Signal.combine(hourS, minuteS, amOrPm).map {
         // TODO Leading zero formating. Use BusTime class if possible
         case (hours, minutes, amOrPm) => s"$hours:$minutes $amOrPm"
       },
       hourPicker,
       minutePicker,
       amPmToggler,
     ))
  }

}
