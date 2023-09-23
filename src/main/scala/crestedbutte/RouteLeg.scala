package crestedbutte

import com.billding.time.MinuteDuration

case class RouteLeg(
  stops: Seq[LocationWithTime]) {
  assert(stops.nonEmpty,
         "Empty Route",
  ) // TODO Upgrade into a true effect

  def trimToStartAt(
    location: Location,
  ): RouteLeg =
    RouteLeg(stops.dropWhile(!_.location.matches(location)))

  def trimToEndAt(
    location: Location,
  ): RouteLeg = {
    val indexOfLastStop =
      stops.indexWhere(_.location.matches(location))
    RouteLeg(stops.take(indexOfLastStop + 1))
  }

  // Assumes non-empty
  def plus(
    location: Location,
    busDuration: MinuteDuration,
  ) =
    RouteLeg(
      stops :+ LocationWithTime(location,
                                stops.last.busTime.plus(busDuration),
      ),
    )
}
