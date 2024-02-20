package crestedbutte

import com.billding.time.MinuteDuration
import zio.json._

object RouteLeg:
  def apply(
    stops: Seq[LocationWithTime],
    routeName: ComponentName,
  ): Either[String, RouteLeg] =
    for
      head <- stops.headOption.toRight("Empty Route")
      last <- stops.lastOption.toRight("Empty Route")
    yield RouteLeg(stops, routeName, head, last)

case class RouteLeg private (
  stops: Seq[LocationWithTime],
  routeName: ComponentName,
  head: LocationWithTime,
  last: LocationWithTime)
    derives JsonCodec {
  lazy val plainTextRepresentation =
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

  def withSameStopsAs(
    other: RouteLeg,
  ): Either[String, RouteLeg] =
    RouteLeg(
      stops.filter(stop =>
        other.stops.exists(locationWithTime =>
          locationWithTime.location == stop.location,
        ),
      ),
      routeName,
    )

  def trimToStartAt(
    location: Location,
  ): Either[String, RouteLeg] =
    RouteLeg(stops.dropWhile(!_.location.matches(location)),
             routeName,
    )

  def trimToEndAt(
    location: Location,
  ): Either[String, RouteLeg] = {
    val indexOfLastStop =
      stops.indexWhere(_.location.matches(location))
    RouteLeg(stops.take(indexOfLastStop + 1), routeName)
  }

  // Assumes non-empty
  def plus(
    location: Location,
    busDuration: MinuteDuration,
  ): RouteLeg =
    copy(
      stops =
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
      routeName,
    )
}
