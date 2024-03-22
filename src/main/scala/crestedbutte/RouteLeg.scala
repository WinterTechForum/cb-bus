package crestedbutte

import com.billding.time.MinuteDuration
import zio.json._

case class RouteSegment(
  r: ComponentName,
  s: LocationWithTime,
  e: LocationWithTime)
    derives JsonCodec {

  // These let us use good names for logical operations, while keeping shortest names for serialization
  val route = r
  val start = s
  val end = e

  lazy val plainTextRepresentation =
    s"""${start.l.name}
       |${start.t.toDumbAmericanString}
       |
       |${end.l.name}
       |${end.t.toDumbAmericanString}
       |""".stripMargin
}

object RouteSegment {
  def fromRouteLeg(
    routeLeg: RouteLeg,
  ): RouteSegment =
    RouteSegment(
      routeLeg.routeName,
      routeLeg.head,
      routeLeg.last,
    )
}

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

  def segmentFrom(start: Location, destination: Location): Option[RouteSegment] =
    for
      startWithTime <- stops.find(_.l == start)
      destinationWithTime <- stops.find(_.l == destination)
    yield
      RouteSegment(routeName, startWithTime, destinationWithTime)

  lazy val plainTextRepresentation =
    val start = stops.head
    val end = stops.last
    s"""${start.l.name}
       |${start.t.toDumbAmericanString}
       |
       |${end.l.name}
       |${end.t.toDumbAmericanString}
       |""".stripMargin

  assert(stops.nonEmpty,
         "Empty Route",
  ) // TODO Upgrade into a true effect

  def withSameStopsAs(
    other: RouteSegment,
  ): Either[String, RouteLeg] =
    RouteLeg(
      stops.filter(stop =>
        // This List is a weird funky artifact from the switch to RouteSegment to RouteLeg
        List(other.start, other.end).exists(locationWithTime =>
          locationWithTime.l == stop.l,
        ),
      ),
      routeName,
    )

  def trimToStartAt(
    location: Location,
  ): Either[String, RouteLeg] =
    RouteLeg(stops.dropWhile(!_.l.matches(location)), routeName)

  def trimToEndAt(
    location: Location,
  ): Either[String, RouteLeg] = {
    val indexOfLastStop =
      stops.indexWhere(_.l.matches(location))
    RouteLeg(stops.take(indexOfLastStop + 1), routeName)
  }

  // Assumes non-empty
  def plus(
    location: Location,
    busDuration: MinuteDuration,
  ): RouteLeg =
    copy(
      stops = stops :+ LocationWithTime(location,
                                        stops.last.t.plus(busDuration),
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
