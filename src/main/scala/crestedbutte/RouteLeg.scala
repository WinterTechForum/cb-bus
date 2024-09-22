package crestedbutte

import com.billding.time.MinuteDuration
import zio.json._

case class RouteSegment(
  r: ComponentName,
  s: LocationWithTime,
  e: LocationWithTime)
    derives JsonCodec {

  assert(
    s.l != e.l,
    "RouteSegment must start and stop at different locations"
  )

  // These let us use good names for logical operations, while keeping shortest names for serialization
  val route = r
  val start = s
  val end = e

  lazy val plainTextRepresentation =
    s"""${start.t.toDumbAmericanString}  ${start.l.name}
       |${end.t.toDumbAmericanString}  ${end.l.name}
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

// TODO Ensure no repeat Locations
case class RouteLeg private (
  stops: Seq[LocationWithTime],
  routeName: ComponentName,
  head: LocationWithTime,
  last: LocationWithTime)
    derives JsonCodec {

  def segmentFrom(
    start: Location,
    destination: Location,
  ): Option[RouteSegment] =
    for
      startWithTime       <- stops.find(_.l.matches(start))
      destinationWithTime <- stops.find(_.l.matches(destination))
    yield RouteSegment(routeName, startWithTime, destinationWithTime)

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
