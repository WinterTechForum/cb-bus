package crestedbutte

import com.billding.time.*
import zio.json.JsonCodec

case class StopTimeInfo(
  time: WallTime,
  waitingDuration: MinuteDuration)

object RouteName {
  private lazy val indexedComponentNames: Seq[(RouteName, Int)] =
    Seq(
      RTA.Southbound.componentName,
      RTA.Northbound.componentName,
    ).zipWithIndex

  def encode(
    name: RouteName,
  ): Int = indexedComponentNames.find(_._1 == name).get._2

  def decode(
    idx: Int,
  ): RouteName = indexedComponentNames.find(_._2 == idx).get._1

  implicit lazy val codec: JsonCodec[RouteName] =
    JsonCodec.int.transform(decode, encode)
}

case class RouteName(
  userFriendlyName: String) {

  val name: String =
    userFriendlyName
      .map((letter: Char) =>
        if (letter.isLetter) letter.toString else "_",
      )
      .mkString

}

import zio.json._
implicit val wallTimeCodec: JsonCodec[WallTime] =
  JsonCodec.int.transform(WallTime.fromMinutes, _.localTime.value)

implicit val hourNotationCodec: JsonCodec[HourNotation] =
  DeriveJsonCodec.gen[HourNotation]

case class Plan(
  l: Seq[RouteSegment])
    derives JsonCodec:

  val routeSegments: Seq[RouteSegment] = l

  val routePieces: Seq[RoutePiece] =
    if (routeSegments.isEmpty)
      routeSegments
    else
      routeSegments.tail.zipWithIndex
        .foldLeft[Seq[RoutePiece]](Seq(routeSegments.head)) {
          case (acc, (next, idx)) =>
            acc.last match
              case RouteSegment(r, s, e, _) =>
                (acc :+ RouteGap(e.t,
                                 next.start.t,
                                 idx * 1000L,
                )) :+ next
              case _ =>
                ??? // Eh, annoying, but not nearly as bad as the previous muck
        }

  val plainTextRepresentation: String =
    l.zipWithIndex
      .map(
        (
          leg,
          idx,
        ) => s"${leg.plainTextRepresentation}",
      )
      .mkString("\n\n") + "\n"

object Plan {
  // TODO Move this to frontend, so that we can move models to common. I think this is the last major hangup.
  import upickle.default.*
  implicit val planRw: ReadWriter[Plan] =
    readwriter[String].bimap[Plan](
      UrlEncoding.encode,
      s =>
        UrlEncoding.decodePlan(s) match
          case Left(error) =>
            throw new Exception(s"Failed to decode Plan: $error")
          case Right(value) => value,
    )

}
object models {
  val allRoutes = List(
    RTA.Southbound.componentName,
    RTA.Northbound.componentName,
  )
}

case class RouteWithTimes(
  legs: Seq[RouteLeg]) {

  private def indexOfLegThatContains(
    other: RouteSegment,
  ) =
    val res = legs.indexWhere(leg =>
      leg.stops.exists(locationWithTime =>
        // TODO Make this more clear
        locationWithTime.t.localTime.value == other.start.t.localTime.value && locationWithTime.l == other.start.l,
      ),
    )
    Option.when(res != -1)(res)

  def nextAfter(
    original: RouteSegment,
  ): Option[RouteSegment] =
    for
      index <- indexOfLegThatContains(original)
      newIndex <-
        Option.when(index + 1 <= legs.size - 1)(
          index + 1,
        )
      newRoute <- legs(newIndex)
        .withSameStopsAs(original)
    yield RouteSegment.fromRouteLegWithId(newRoute, original.id)

  def allNextAfter(
    original: RouteSegment,
  ): Seq[RouteSegment] =
    nextAfter(original) match {
      case Some(next) =>
        allNextAfter(next) :+ next
      case None => Seq.empty
    }

  def nextBefore(
    original: RouteSegment,
  ): Option[RouteSegment] =
    for
      index <- indexOfLegThatContains(original)
      newIndex <-
        Option.when(index - 1 >= 0)(
          index - 1,
        )
      newRoute <- legs(newIndex)
        .withSameStopsAs(original)
      res = RouteSegment.fromRouteLegWithId(newRoute, original.id)
    yield res

  def allNextBefore(
    original: RouteSegment,
  ): Seq[RouteSegment] =
    nextBefore(original) match {
      case Some(next) =>
        next +: allNextBefore(next)
      case None => Seq.empty
    }

  def allRouteSegmentsWithSameStartAndStop(
    original: RouteSegment,
  ): Seq[RouteSegment] =
    allNextBefore(original).reverse ++ Seq(original) ++ allNextAfter(
      original,
    ).reverse

  val allStops: Seq[BusScheduleAtStop] =
    legs.foldLeft(Seq[BusScheduleAtStop]()) { case (acc, leg) =>
      leg.stops.foldLeft(acc) { case (innerAcc, stop) =>
        if (innerAcc.exists(_.location == stop.l))
          innerAcc.map {
            // TODO Confirm where we are getting routeName
            case BusScheduleAtStop(location, times, routeName)
                if location == stop.l =>
              BusScheduleAtStop(location,
                                times :+ stop.t,
                                leg.routeName,
              )
            case other => other
          }
        else
          innerAcc :+ BusScheduleAtStop(stop.l,
                                        Seq(stop.t),
                                        leg.routeName,
          )
      }
    }

}

object RouteWithTimes {
  def lookupRouteWithTimes(
    routeName: RouteName,
  ): RouteWithTimes = {

    import crestedbutte.RTA.{Northbound, Southbound}
    routeName match {
      case Southbound.componentName =>
        Southbound.fullSchedule.routeWithTimes
      case Northbound.componentName =>
        Northbound.fullSchedule.routeWithTimes
      case _ =>
        throw new IllegalArgumentException(
          s"Unknown route: $routeName",
        )
    }
  }

  def sched(
    routeName: RouteName,
    location: Location,
    routeConstructor: RouteLeg => RouteLeg,
    stopTimes: String*,
  ): RouteWithTimes = {
    val stopTimesTyped =
      stopTimes
        .flatMap(time =>
          RouteLeg(Seq(LocationWithTime(location, WallTime(time))),
                   routeName,
          ),
        )
    RouteWithTimes(
      stopTimesTyped
        .map(routeConstructor),
    )

  }

}

import MinuteDuration.* // TODO Just for the duration syntax
object RTA {
  object Northbound {
    val componentName = RouteName("Rta Northbound")

    def constructNormalRoute(
      routeLeg: RouteLeg,
    ): RouteLeg =
      routeLeg
        .plus(Location.GunnisonLibrary, 3.minutes)
        .plus(Location.GunnisonCommunitySchools, 1.minutes)
        .plus(Location.EleventhAndVirginia, 2.minutes)
        .plus(Location.Safeway, 2.minutes)
        .plus(Location.TellerAndHighwayFifty, 3.minutes)
        .plus(Location.Western, 2.minutes)
        .plus(Location.DenverAndHighwayOneThirtyFive, 4.minutes)
        .plus(Location.SpencerAndHighwayOneThirtyFive, 2.minutes)
        .plus(Location.TallTexan, 2.minutes)
        .plus(Location.OhioCreek, 1.minutes)
        .plus(Location.Almont, 7.minutes)
        .plus(Location.CBSouth, 16.minutes)
        .plus(Location.Riverland, 6.minutes)
        .plus(Location.BrushCreek, 1.minutes)
        .plus(Location.Riverbend, 1.minutes)
        .plus(Location.FourWayUphill, 5.minutes)
        .plus(Location.MountaineerSquare, 8.minutes)

    val normalRouteWithTimes =
      RouteWithTimes.sched(
        Northbound.componentName,
        Location.RecCenter,
        constructNormalRoute,
        "5:21 AM",
        "5:51 AM",
        "6:21 AM",
        "6:56 AM",
        "7:26 AM",
        "7:56 AM",
        "8:26 AM",
        "9:01 AM",
        "9:31 AM",
        "10:01 AM",
        "10:31 AM",
        "11:01 AM",
        "11:36 AM",
        "12:06 PM",
        "12:36 PM",
        "1:06 PM",
        //
        "1:41 PM",
        "2:11 PM",
        "2:41 PM",
        "3:11 PM",
        "3:46 PM",
        "4:16 PM",
        "4:46 PM",
        "5:16 PM",
        "5:46 PM",
        "6:16 PM",
        "6:51 PM",
        "7:21 PM",
        "7:51 PM",
        "8:21 PM",
        "8:56 PM",
        "9:26 PM",
        "9:56 PM",
      )

    val fullSchedule = NamedRoute(
      RouteName("Rta Northbound"),
      normalRouteWithTimes,
    )
  }

  object Southbound {
    val componentName = RouteName("Rta Southbound")

    val normalRouteWithTimes =
      RouteWithTimes.sched(
        Southbound.componentName,
        Location.MountaineerSquare,
        constructNormalRoute,
        "6:40 AM",
        "7:10 AM",
        "7:40 AM",
        "8:15 AM",
        "8:45 AM",
        "9:15 AM",
        "9:45 AM",
        "10:20 AM",
        "10:50 AM",
        "11:20 AM",
        "11:50 AM",
        "12:20 PM",
        "12:55 PM",
        "01:25 PM",
        "01:55 PM",
        "02:25 PM",
        "03:00 PM",
        "03:30 PM",
        "04:00 PM",
        "04:30 PM",
        "05:05 PM",
        "05:35 PM",
        "06:05 PM",
        "06:35 PM",
        "07:05 PM",
        "07:35 PM",
        "08:10 PM",
        "08:40 PM",
        "09:10 PM",
        "09:40 PM",
        "10:20 PM",
        "11:00 PM",
        "11:30 PM",
      )

    def constructNormalRoute(
      routeLeg: RouteLeg,
    ): RouteLeg = {
      val basicRoute =
        routeLeg
          .plus(Location.FourwayGunnison, 8.minutes)
          .plus(Location.Riverbend, 2.minutes)
          .plus(Location.BrushCreek, 1.minutes)
          .plus(Location.Riverland, 1.minutes)
          .plus(Location.CBSouth, 8.minutes)
          .plus(Location.Almont, 14.minutes)
          .plus(Location.OhioCreek, 8.minutes)
          .plus(Location.TallTexan, 1.minutes)
          // TODO What to do here for the weird off-set times?
          .plus(Location.RecCenter, 3.minutes)
          .plus(Location.GunnisonLibrary, 3.minutes)
          .plus(Location.GunnisonCommunitySchools, 1.minutes)
          .plus(Location.EleventhAndVirginia, 2.minutes)
          .plus(Location.Safeway, 2.minutes)
          .plus(Location.TellerAndHighwayFifty, 3.minutes)
          .plus(Location.Western, 2.minutes)
          .plus(Location.DenverAndHighwayOneThirtyFive, 3.minutes)

      val (recCenterStop, recCenterIndex) =
        basicRoute.stops.zipWithIndex
          .find(_._1.l == Location.RecCenter)
          .getOrElse(
            throw new Exception("Could not find RecCenter?!"),
          )
      if (
        Northbound.fullSchedule.routeWithTimes.legs
          .exists(leg => leg.stops.contains(recCenterStop))
      ) basicRoute
      else {
        // TODO There is still weirdness here in how the time is reported.
        //  If you get on at tall texan, heading south, you might arrive a few minutes later than advertised.
        //  This is incorrect, but it's a lesser evil than having someone show up at the rec center 5 minutes late to catch the bus that leaves from there.
        //  It's just part of the weirdness of the RTA schedule.
        val (stopsBeforeRecCenter, stopsAfterRecCenter) =
          basicRoute.stops.splitAt(
            recCenterIndex,
          ) // TODO Check off-by-1 issues

        val shiftedStops =
          stopsAfterRecCenter.map(locationWithTime =>
            locationWithTime.copy(t =
              locationWithTime.t.plusMinutes(-5),
            ),
          )

        val updatedStops: Seq[LocationWithTime] =
          stopsBeforeRecCenter ++ shiftedStops

        RouteLeg(
          updatedStops,
          RouteName("Rta Southbound"),
        ).getOrElse(
          throw new Exception(
            "No idea wtf happened with this RouteLeg",
          ),
        )

      }
    }

    val fullSchedule = NamedRoute(
      RouteName("Rta Southbound"),
      normalRouteWithTimes,
    )
  }

}

trait RoutePiece {
  val id: Long
}

val random = new java.util.Random()

case class RouteGap(
  start: WallTime,
  endTime: WallTime,
  id: Long)
    extends RoutePiece

case class RouteSegment private (
  r: RouteName,
  s: LocationWithTime,
  e: LocationWithTime,
  id: Long)
    extends RoutePiece
    derives JsonCodec {

  assert(
    s.l != e.l,
    "RouteSegment must start and stop at different locations",
  )

  // These let us use good names for logical operations, while keeping shortest names for serialization
  val route = r
  val start = s
  val end = e

  val routeWithTimes: RouteWithTimes =
    RouteWithTimes.lookupRouteWithTimes(r)

  lazy val plainTextRepresentation =
    s"""${start.t.toDumbAmericanString}  ${start.l.name}
       |${end.t.toDumbAmericanString}  ${end.l.name}""".stripMargin

      /** TODO This all feels awkward.
        *
        * @param lwt
        * @param previousStartTime
        *   this ensures we only update one entry
        * @param previousEndTime
        * @return
        */
  def updateTimeAtLocation(
    lwt: LocationWithTime,
    previousStartTime: WallTime,
    previousEndTime: WallTime,
  ): RouteSegment =
    lwt.l match
      case s.l if s.t == previousStartTime =>
        copy(s = s.copy(t = lwt.t), id = id)
      case e.l if e.t == previousEndTime =>
        copy(e = e.copy(t = lwt.t), id = id)
      case other => this
}

object RouteSegment {
  def attempt(
    r: RouteName,
    s: LocationWithTime,
    e: LocationWithTime,
  ) =
    for {
      firstPass <- Either.cond(
        s.l != e.l,
        RouteSegment(r, s, e, random.nextLong()),
        "Either: RouteSegment must start and stop at different locations",
      )

      secondPass <- Either.cond(
        WallTime.ordering.compare(s.t, e.t) <= 0,
        RouteSegment(r, s, e, random.nextLong()),
        "Either: RouteSegment must be in correct order. Start: " + s + "  End: " + e,
      )
    } yield secondPass

  def fromRouteLeg(
    routeLeg: RouteLeg,
  ): RouteSegment =
    RouteSegment(
      routeLeg.routeName,
      routeLeg.head,
      routeLeg.last,
      random.nextLong(),
    )

  def fromRouteLegWithId(
    routeLeg: RouteLeg,
    id: Long,
  ): RouteSegment =
    RouteSegment(
      routeLeg.routeName,
      routeLeg.head,
      routeLeg.last,
      id,
    )
}

object RouteLeg:
  def apply(
    stops: Seq[LocationWithTime],
    routeName: RouteName,
  ): Option[RouteLeg] =
    for
      head <- stops.headOption
      last <- stops.lastOption
    yield RouteLeg(stops, routeName, head, last)

// TODO Ensure no repeat Locations
case class RouteLeg private (
  stops: Seq[LocationWithTime],
  routeName: RouteName,
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
      res <- RouteSegment
        .attempt(routeName, startWithTime, destinationWithTime)
        .toOption
    yield res

  assert(stops.nonEmpty,
         "Empty Route",
  ) // TODO Upgrade into a true effect

  def withSameStopsAs(
    other: RouteSegment,
  ): Option[RouteLeg] =
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

case class NamedRoute(
  componentName: RouteName,
  routeWithTimes: RouteWithTimes) {

  val allStops: Seq[Location] =
    routeWithTimes.legs.head.stops.map(_.l)

  def segment(
    start: Location,
    end: Location,
  ): Option[Seq[RouteSegment]] =
    val result =
      routeWithTimes.legs
        .flatMap { leg =>
          leg.segmentFrom(start, end)
        }
        .filter(segment =>
          WallTime.ordering
            .compare(segment.start.t, segment.end.t) <= 0,
        )
    Option.when {
      result.nonEmpty
    } {
      result
    }
}

case class LocationWithTime(
  l: Location,
  t: WallTime)
    derives JsonCodec

case class BusScheduleAtStop(
  location: Location,
  times: Seq[WallTime],
  routeName: RouteName) {
  val locationsWithTimes =
    times.map(t => LocationWithTime(location, t))

}

object CompleteStopList {
  val values =
    Seq(
      Location.MountaineerSquare,
      Location.FourwayGunnison,
      Location.Riverbend,
      Location.BrushCreek,
      Location.Riverland,
      Location.CBSouth,
      Location.Almont,
      Location.OhioCreek,
      Location.TallTexan,
      Location.RecCenter,
      Location.GunnisonLibrary,
      Location.GunnisonCommunitySchools,
      Location.EleventhAndVirginia,
      Location.Safeway,
      Location.TellerAndHighwayFifty,
      Location.Western,
      Location.DenverAndHighwayOneThirtyFive,
      Location.SpencerAndHighwayOneThirtyFive,
    )

}

enum ServiceWorkerAction derives JsonCodec:
  case StartNotifications
  case StopNotifications
  case UpdatePlan
  case TestNotify

case class ServiceWorkerMessage(
  action: ServiceWorkerAction,
  plan: Option[
    Plan,
  ]) // TODO Should actually be a different type of message
    derives JsonCodec
