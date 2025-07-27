package crestedbutte.url_routing

import com.billding.time.WallTime
import crestedbutte.laminar.{AppMode, Components}
import urldsl.errors.DummyError
import urldsl.language.QueryParameters
import java.time.ZoneId

import java.time.OffsetDateTime

import upickle.default.*
import crestedbutte.Plan
import com.raquo.laminar.api.L
import com.raquo.laminar.api.L.*
import com.raquo.waypoint.*
import crestedbutte.UrlEncoding

case class BusPage(
  mode: AppMode,
  time: Option[WallTime],
  plan: Option[Plan]) {

  val fixedTime = time

  val javaClock =
    if (fixedTime.isDefined)
      java.time.Clock.fixed(
        OffsetDateTime
          .parse(
            s"2025-02-21T${fixedTime.get.toEUString}:00.00-07:00",
          )
          .toInstant,
        ZoneId.systemDefault(),
      )
    else
      java.time.Clock.systemDefaultZone()
}

object BusPage {
  implicit val rw: ReadWriter[BusPage] = macroRW

  val encodePage: BusPage => (
    Option[String],
    Option[String],
    Option[String],
  ) =
    page =>
      (Some(page.mode.toString),
       page.time.map(_.toEUString),
       page.plan.map(UrlEncoding.encode),
      )

  val decodePage = {
    (
      mode: Option[String],
      time: Option[String],
      plan: Option[String],
    ) =>
      BusPage(
        mode =
          mode.map(AppMode.valueOf).getOrElse(AppMode.Production),
        time = time.map(WallTime.apply),
        plan = plan.flatMap(UrlEncoding.decode(_).toOption),
      )
  }.tupled

}
