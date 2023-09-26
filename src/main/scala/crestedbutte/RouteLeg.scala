package crestedbutte

import com.billding.time.MinuteDuration
import zio.json._

case class RouteLeg(
  stops: Seq[LocationWithTime])
    derives JsonCodec {
  val plainTextRepresentation =
    val start = stops.head
    val end = stops.last
    s"""${start.location.name}
       |${start.busTime.toDumbAmericanString}
       |
       |${end.location.name}
       |${end.busTime.toDumbAmericanString}
       |""".stripMargin

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

  def ends =
    RouteLeg(
      Seq(
        stops.head,
        stops.last,
      ),
    )
}
