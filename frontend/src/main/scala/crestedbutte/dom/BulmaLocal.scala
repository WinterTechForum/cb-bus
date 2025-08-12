package crestedbutte.dom

import animus.Animation
import com.raquo.laminar.api.L.*
import com.raquo.laminar.nodes.ReactiveHtmlElement
import crestedbutte.*
import crestedbutte.laminar.LocationTimeDirection

sealed trait StopContext
object StopContext {
  case object Departure extends StopContext
  case object Arrival extends StopContext
}

object BulmaLocal {
  def locationwithTime(
    l: LocationWithTime,
    segment: RouteSegment,
    updates: Sink[LocationTimeDirection],
  ) = {
    val buttonModifier =
      if (segment.start == l || segment.end == l)
        " is-primary"
      else
        " is-info"
    div(
      if (segment.start == l || segment.end == l)
        idAttr := "selected-time"
      else
        cls := "",
      textAlign := "center",
      paddingBottom := "3px",
      cls := "time",
      button(
        cls := s"button clickable-time button m-2 $buttonModifier",
        onClick.mapTo {
          LocationTimeDirection(l, segment)
        } --> updates,
        l.t.toDumbAmericanString,
      ),
    )
  }

  def UpcomingStops(
    scheduleAtStop: BusScheduleAtStop,
    routeSegment: RouteSegment,
    selectedTimeUpdater: Sink[LocationTimeDirection],
    context: StopContext,
  ) = {
    val $opacity = Animation.from(0).wait(250).to(1).run
    val $width = Animation.from(0).wait(250).to(100).run
    div(
      width <-- $width.map(_.toString + "%"),
      opacity <-- $opacity,
      div(
        // Keeps this text at the top while we scroll through the stops
        position := "sticky",
        top := "0",
        backgroundColor := "white",
        zIndex := "10",
        paddingBottom := "10px",
        h5(textAlign := "center",
           context match {
             case StopContext.Arrival   => s"Arriving at "
             case StopContext.Departure => s"Leave from "
           },
        ),
        h2(
          textAlign := "center",
          fontWeight := "bold",
          fontSize := "2.0rem",
          color := "#363636",
          textShadow := "0 1px 3px rgba(0,0,0,0.2)",
          scheduleAtStop.location.name,
        ),
        h5(textAlign := "center", "at"),
      ),
      div(
        // maxHeight := "70vh", // TODO I don't know if this is safe if the top portion is taking more than 30 VH.
        // overflowY := "display",
        scheduleAtStop.locationsWithTimes.map { l =>
          locationwithTime(l, routeSegment, selectedTimeUpdater)
        },
      ),
    )
  }

}
