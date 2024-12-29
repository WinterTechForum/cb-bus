package crestedbutte.laminar

import com.billding.time.{MinuteDuration, WallTime}
import com.raquo.laminar.api.L.*
import crestedbutte.*
import crestedbutte.NotificationStuff.desiredAlarms
import crestedbutte.dom.BulmaLocal
import crestedbutte.dom.BulmaLocal.UpcomingStops
import crestedbutte.laminar.Experimental.getLocation
import crestedbutte.laminar.TouchControls.Swipe
import crestedbutte.routes.{
  CompleteStopList,
  RouteWithTimes,
  RtaNorthbound,
  RtaSouthbound,
}
import org.scalajs.dom
import org.scalajs.dom.{HTMLAnchorElement, HTMLDivElement}

import java.time.format.DateTimeFormatter
import java.time.{Clock, Instant, OffsetDateTime}
import scala.concurrent.duration.FiniteDuration
import animus._

object SvgIcon {

  def apply(
    name: String,
    clsName: String = "",
  ) =
    img(
      cls := s"glyphicon $clsName",
      src := s"/glyphicons/svg/individual-svg/$name",
      alt := "Thanks for riding the bus!",
    )

}
