package crestedbutte.laminar

import com.billding.time.WallTime
import com.raquo.airstream.core.Signal
import com.raquo.laminar.nodes.ReactiveHtmlElement
import com.raquo.laminar.api.L.*
import java.time.format.DateTimeFormatter
import java.time.{Clock, OffsetDateTime}
import scala.concurrent.duration.FiniteDuration

class FrontEndClock(
  javaClock: Clock) {

  private val clockTicks = new EventBus[Unit]

  private def currentWallTime(
    javaClock: Clock,
  ) =
    WallTime(
      OffsetDateTime
        .now(javaClock)
        .toLocalTime
        .format(
          DateTimeFormatter.ofPattern("HH:mm"),
        ),
    )

  private val initialTime =
    currentWallTime:
      javaClock

  val timeStamps: Signal[WallTime] = clockTicks.events
    .map(_ => currentWallTime(javaClock))
    .startWith(initialTime)

  val clockElement =
    RepeatingElement()
      .repeatWithInterval( // This acts like a Dune thumper
        (),
        new FiniteDuration(10,
                           scala.concurrent.duration.SECONDS,
        ), // TODO Make low again
      ) --> clockTicks
}
