package crestedbutte

import com.billding.time.*
import crestedbutte.routes.RTA
import crestedbutte.routes.RouteWithTimes
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

case class LateNightRecommendation( // TODO Rename "LateNight" or something
  message: String,
  phoneNumber: String = "970-209-0519")

case class GpsCoordinates(
  latitude: Double,
  longitude: Double)

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
                // TODO Would be nice to make the gaps in a way such that they don't get re-rendered when the plan changes
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
      .mkString("\n")

object Plan {
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
