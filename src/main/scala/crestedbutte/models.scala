package crestedbutte

import com.billding.time.*

case class StopTimeInfo(
  time: WallTime,
  waitingDuration: MinuteDuration)

case class BusTimeWithLocation(
  busTime: WallTime,
  location: Location)

enum RouteMode(
  name: String) {
  // TODO Check ordering of all coordinates
  case Active extends RouteMode("Active")
  case Hidden extends RouteMode("Hidden")
}

case class PhoneNumber(
  number: String,
  name: String)

// TODO General name
case class ComponentName(
  userFriendlyName: String) {

  val name: String =
    userFriendlyName
      .map((letter: Char) =>
        if (letter.isLetter) letter.toString else "_",
      )
      .mkString

  def elementNameMatches(
    elementName: String,
  ) = name == elementName
  // TODO Check ordering of all coordinates
  /*
  val TownLoop: Val = Val()

  val ThreeSeasonsLoop: Val =
    Val()

   */

}

case class LateNightRecommendation( // TODO Rename "LateNight" or something
  message: String,
  phoneNumber: String = "970-209-0519")

case class GpsCoordinates(
  latitude: Double,
  longitude: Double)

case class UpcomingArrivalInfoWithFullSchedule(
  upcomingArrivalInfo: UpcomingArrivalInfo,
  busScheduleAtStop: BusScheduleAtStop,
  namedRoute: NamedRoute, // TODO
) {}

case class UpcomingArrivalInfo(
  location: Location,
  content: Either[StopTimeInfo, LateNightRecommendation],
  /* TODO: waitDuration: Duration*/)

object UpcomingArrivalInfo {

  def apply(
    location: Location,
    content: StopTimeInfo,
  ): UpcomingArrivalInfo =
    UpcomingArrivalInfo(
      location,
      Left(
        content,
      ),
    )

  def apply(
    location: Location,
    content: LateNightRecommendation,
  ): UpcomingArrivalInfo =
    UpcomingArrivalInfo(
      location,
      Right(content),
    )

}

case class UpcomingArrivalComponentData(
  upcomingArrivalInfoForAllRoutes: Seq[
    UpcomingArrivalInfoWithFullSchedule,
  ],
  routeName: ComponentName)

import zio.json._

implicit val wallTimeCodec: JsonCodec[WallTime] =
  DeriveJsonCodec.gen[WallTime]
implicit val minutesCodec: JsonCodec[Minutes] =
  DeriveJsonCodec.gen[Minutes]
implicit val hourNotationCodec: JsonCodec[HourNotation] =
  DeriveJsonCodec.gen[HourNotation]

case class Plan(
  legs: Seq[RouteLeg])
    derives JsonCodec:
  val plainTextRepresentation: String =
    legs.zipWithIndex
      .map(
        (
          leg,
          idx,
        ) => s"Trip ${idx + 1}\n\n" + leg.plainTextRepresentation,
      )
      .mkString("\n\n")
