package crestedbutte

import com.billding.time.*
import crestedbutte.routes.{RtaNorthbound, RtaSouthbound}
import zio.json.JsonCodec

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

object ComponentName {
//  implicit val codec: JsonCodec[ComponentName] =
//    JsonCodec.string.transform(ComponentName.apply,
//                               _.userFriendlyName,
//    )

  private lazy val indexedComponentNames: Seq[(ComponentName, Int)] =
    Seq(
      RtaSouthbound.componentName,
      RtaNorthbound.componentName,
    ).zipWithIndex

  def encode(
    name: ComponentName,
  ): Int = indexedComponentNames.find(_._1 == name).get._2

  def decode(
    idx: Int,
  ): ComponentName = indexedComponentNames.find(_._2 == idx).get._1

  implicit lazy val codec: JsonCodec[ComponentName] =
    JsonCodec.int.transform(decode, encode)
}
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
}

case class LateNightRecommendation( // TODO Rename "LateNight" or something
  message: String,
  phoneNumber: String = "970-209-0519")

case class GpsCoordinates(
  latitude: Double,
  longitude: Double)

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

import zio.json._
implicit val wallTimeCodec: JsonCodec[WallTime] =
  JsonCodec.int.transform(WallTime.fromMinutes, _.localTime.value)

implicit val hourNotationCodec: JsonCodec[HourNotation] =
  DeriveJsonCodec.gen[HourNotation]

case class Plan(
  // TODO It kind of sucks that I'm working with the compressed name here.
  l: Seq[RouteSegment])
    derives JsonCodec:
  
  val plainTextRepresentation: String =
    l.zipWithIndex
      .map(
        (
          leg,
          idx,
        ) => s"${leg.plainTextRepresentation}",
      )
      .mkString("\n")
