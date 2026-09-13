package crestedbutte.laminar

import com.raquo.laminar.api.L.*
import com.billding.time.WallTime
import crestedbutte.*
import crestedbutte.pwa.Persistence

object StopPicker {
  def region(stop: Location): String = stop match {
    case Location.MountaineerSquare => "Mt. Crested Butte"
    case Location.FourWayUphill | Location.FourwayGunnison | Location.Riverbend => "Crested Butte"
    case Location.BrushCreek | Location.Riverland | Location.CBSouth | Location.Almont |
         Location.OhioCreek | Location.TallTexan => "Along the valley"
    case _ => "Gunnison"
  }

  def matches(stop: Location, query: String): Boolean =
    s"${stop.name} ${stop.altName} ${region(stop)}".toLowerCase.contains(query.trim.toLowerCase)

  def apply(locations: Seq[Location], db: Persistence, initial: Option[Location], now: Signal[WallTime],
            reachable: (Location, Location, WallTime) => Boolean,
            choose: (Location, Location) => Unit, hasSaved: Signal[Boolean],
            loadSaved: () => Unit, cancel: () => Unit, canCancel: Boolean): HtmlElement = {
    val origin = Var(initial)
    val query = Var("")
    val favorites = Var(db.stopPreferences("stops:favorites"))
    val recent = db.stopPreferences("stops:recent")
    val groups = query.signal.combineWith(favorites.signal).map { case (q, starred) =>
      val found = locations.filter(matches(_, q))
      if (q.trim.nonEmpty) Seq("Search results" -> found)
      else {
        val preferred = found.filter(starred.contains)
        val recentStops = recent.filter(s => found.contains(s) && !preferred.contains(s))
        val remaining = found.filterNot(s => preferred.contains(s) || recentStops.contains(s))
        Seq("Favorites" -> preferred, "Recent stops" -> recentStops) ++
          Seq("Mt. Crested Butte", "Crested Butte", "Along the valley", "Gunnison")
            .map(name => name -> remaining.filter(region(_) == name))
      }
    }
    div(cls := "stop-picker",
      div(cls := "selector-toolbar",
        button(cls := "text-button", "Saved trips",
          display <-- hasSaved.map(has => if (has) "inline-flex" else "none"),
          onClick --> Observer { _ => loadSaved() }),
        Option.when(canCancel)(button(cls := "text-button", "Cancel", onClick --> Observer { _ => cancel() }))),
      h2(child.text <-- origin.signal.map(o => if (o.isDefined) "Select your destination" else "Select your origin")),
      child <-- origin.signal.map {
        case Some(stop) => div(cls := "selected-origin",
          span(s"From: ${stop.name}"),
          button(cls := "text-button", "Change", onClick --> Observer { _ => origin.set(None); query.set("") }))
        case None => p(cls := "muted", "Choose a stop. Tap ☆ to keep a favorite on this device.")
      },
      input(cls := "stop-search", typ := "search", aria.label := "Search stops or landmarks",
        placeholder := "Search stops or landmarks", value <-- query.signal,
        onInput.mapToValue --> query.writer),
      child <-- groups.map { sections =>
        if (sections.forall(_._2.isEmpty)) p(role := "status", "No stops match. Try a stop, landmark or town.")
        else div(sections.filter(_._2.nonEmpty).map { case (name, stops) =>
          div(cls := "stop-group", h3(name),
            stops.map { stop =>
              val unavailable = origin.signal.combineWith(now).map {
                case (Some(start), time) if start != stop => !reachable(start, stop, time)
                case _ => false
              }
              div(cls := "stop-row",
                button(cls := "stop-choice",
                  idAttr := s"stop-btn-${stop.name.replace(" ", "-")}",
                  disabled <-- unavailable,
                  cls.toggle("selected-starting-point") <-- origin.signal.map(_.contains(stop)),
                  span(cls := "stop-name", stop.name),
                  Option.when(stop.altName.nonEmpty)(span(cls := "stop-subtitle", stop.altName.stripPrefix("(").stripSuffix(")"))),
                  child <-- unavailable.map(u => if (u) span(cls := "stop-unavailable", "No direct departure for this trip") else emptyNode),
                  onClick --> Observer { _ =>
                    origin.now() match {
                      case None => origin.set(Some(stop)); query.set("")
                      case Some(start) if start == stop => origin.set(None); query.set("")
                      case Some(start) => choose(start, stop)
                    }
                  }),
                button(cls := "favorite-stop",
                  aria.label <-- favorites.signal.map(f => s"${if (f.contains(stop)) "Unfavorite" else "Favorite"} ${stop.name}"),
                  aria.pressed <-- favorites.signal.map(_.contains(stop).toString),
                  child.text <-- favorites.signal.map(f => if (f.contains(stop)) "★" else "☆"),
                  onClick --> Observer { _ =>
                    favorites.update(f => if (f.contains(stop)) f.filterNot(_ == stop) else f :+ stop)
                    db.saveStopPreferences("stops:favorites", favorites.now())
                  }),
              )
            })
        })
      },
    )
  }
}
