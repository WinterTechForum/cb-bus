package crestedbutte.url_routing

import com.billding.time.WallTime
import crestedbutte.laminar.{AppMode, Components}
import urldsl.errors.DummyError
import urldsl.language.QueryParameters
import java.time.ZoneId

import java.time.OffsetDateTime

import upickle.default.*
import crestedbutte.Plan

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
            s"2020-02-21T${fixedTime.get.toEUString}:00.00-07:00",
          )
          .toInstant,
        ZoneId.systemDefault(),
      )
    else
      java.time.Clock.systemDefaultZone()
}

object BusPage {
  implicit val rw: ReadWriter[BusPage] = macroRW
}
