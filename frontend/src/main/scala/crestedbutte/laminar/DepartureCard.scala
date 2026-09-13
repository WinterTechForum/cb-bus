package crestedbutte.laminar

import com.raquo.laminar.api.L.*
import com.billding.time.WallTime
import crestedbutte.*

object DepartureCard {
  def apply(segment: RouteSegment, now: Signal[WallTime], dateLabel: Signal[String],
            update: Observer[RouteSegment], remove: Observer[RouteSegment],
            move: Observer[Int], position: Signal[(Boolean, Boolean)]): HtmlElement = {
    val editing = Var(false)
    val expanded = Var(false)
    val state = now.combineWith(dateLabel).map { case (time, date) =>
      val departure = segment.start.t.localTime.value - time.localTime.value
      val finished = date != "Today" || segment.end.t.isBefore(time)
      val label = if (date != "Today") s"Planned for $date"
        else if (finished) "Scheduled ride ended"
        else if (departure < 0) "Scheduled ride in progress"
        else if (departure == 0) "Departs now"
        else s"Bus in $departure min"
      (label, finished)
    }
    div(
      cls := "plan-segments departure-card",
      div(cls := "departure-card-heading",
        strong(child.text <-- state.map(_._1)),
        span(cls := "schedule-label", "Scheduled")),
      div(cls := "departure-route", s"${segment.start.l.name} → ${segment.end.l.name}"),
      button(cls := "text-button",
        display <-- state.map(s => if (s._2) "inline-flex" else "none"),
        child.text <-- expanded.signal.map(e => if (e) "Hide details" else "Show details"),
        aria.expanded <-- expanded.signal,
        onClick --> Observer { _ => expanded.update(!_) }),
      div(
        display <-- state.combineWith(expanded.signal).map { case (_, finished, show) =>
          if (!finished || show) "block" else "none"
        },
        div(cls := "departure-times",
          span(s"Depart ${segment.start.t.toDumbAmericanString}"),
          span(s"Arrive ${segment.end.t.toDumbAmericanString}")),
        div(cls := "ride-actions",
          button(cls := "text-button", "Change time", onClick --> Observer { _ => editing.set(true) }),
          button(cls := "text-button", "Remove ride", onClick --> Observer { _ => remove.onNext(segment) }),
          div(cls := "ride-reorder",
            display <-- position.map(p => if (p._1 && p._2) "none" else "flex"),
            button(cls := "text-button", "Move up", disabled <-- position.map(_._1),
              onClick --> Observer { _ => move.onNext(-1) }),
            button(cls := "text-button", "Move down", disabled <-- position.map(_._2),
              onClick --> Observer { _ => move.onNext(1) }))),
      ),
      child <-- editing.signal.map {
        case false => emptyNode
        case true =>
          val options = TripPlanning.departures(segment)
          val selected = Var(options.find(_.start.t == segment.start.t).getOrElse(options.headOption.getOrElse(segment)))
          val (wheel, selection) = ScrollingWheel.ScrollingWheel(options,
            (s: RouteSegment) => div(s"${s.start.t.toDumbAmericanString} → ${s.end.t.toDumbAmericanString}"),
            initialSelectedElement = Some(selected.now()), externalSelection = Some(selected.signal))
          ModalSheet(editing, "Change departure", div(
            p(s"${segment.start.l.name} → ${segment.end.l.name}"),
            p(cls := "muted", "Times are Mountain Time. Scroll or choose a departure below."),
            Option.when(options.nonEmpty)(wheel),
            if (options.nonEmpty) selection --> selected.writer else emptyMod,
            Option.when(options.isEmpty)(p(role := "alert", "This ride is not in the current timetable. Choose new stops to replace it.")),
            select(cls := "departure-select", aria.label := "Departure time",
              value <-- selected.signal.map(_.start.t.toEUString),
              options.map(s => option(value := s.start.t.toEUString,
                s"${s.start.t.toDumbAmericanString} → ${s.end.t.toDumbAmericanString}")),
              onChange.mapToValue --> Observer[String] { value =>
                options.find(_.start.t.toEUString == value).foreach(selected.set)
              }),
            button(cls := "button primary-action", "Use this departure",
              disabled := options.isEmpty,
              onClick --> Observer { _ =>
                editing.set(false)
                update.onNext(selected.now().withId(segment.id))
              }),
          ))
      },
    )
  }
}
