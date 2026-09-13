package crestedbutte

import com.billding.time.WallTime

object TripPlanning {
  def departures(segment: RouteSegment): Seq[RouteSegment] =
    segment.routeWithTimes.legs.flatMap(_.segmentFrom(segment.start.l, segment.end.l))
      .sortBy(_.start.t.localTime.value)

  /** Reuse a trip today without changing its stops, direction, IDs or stopovers.
    * Never wrap to an earlier departure or return a partially updated plan.
    */
  def useNow(plan: Plan, now: WallTime): Either[String, Plan] = {
    plan.routeSegments.zipWithIndex.foldLeft[Either[String, Vector[RouteSegment]]](Right(Vector.empty)) {
      case (result, (segment, index)) => result.flatMap { updated =>
        val pause = if (index == 0) 0 else {
          val previous = plan.routeSegments(index - 1)
          val gap = segment.start.t.localTime.value - previous.end.t.localTime.value
          if (gap < 0) gap + 1440 else gap
        }
        val cutoff = updated.lastOption
          .map(_.end.t.localTime.value + pause)
          .getOrElse(now.localTime.value + 1)
        val next = departures(segment)
          .find(s => s.start.t.localTime.value >= cutoff &&
            s.end.t.localTime.value >= s.start.t.localTime.value)
        next.toRight(s"No departure left today from ${segment.start.l.name} that fits this trip. Your trip has not changed.")
          .map(s => updated :+ s.withId(segment.id))
      }
    }.map(segments => Plan(segments))
  }
}
