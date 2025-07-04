package crestedbutte

import zio.json._

enum Location(
  val name: String,
  val altName: String = "",
  val gpsCoordinates: Option[GpsCoordinates] = None) {

  case FourWayUphill
      extends Location(
        "4-way",
        "(To Mountain)",
        Some(GpsCoordinates(38.870355, -106.980905)),
      )

  case MountaineerSquare
      extends Location(
        "Mountaineer Square",
        "(CBMR)",
        Some(GpsCoordinates(38.900902, -106.966650)),
      )

  case FourwayGunnison
      extends Location(
        "4-way",
        "(To Gunnison)",
      )

  // RTA stops
  case GunnisonCommunitySchools
      extends Location(
        "Community Schools",
        altName = "(Gunnison)",
      )

  case GunnisonLibrary
      extends Location(
        "Gunnison Library",
      )

  case EleventhAndVirginia extends Location("Eleventh & Virgina")

  case Safeway extends Location("Safeway", "(Spruce & Highway 50)")

  case TellerAndHighwayFifty
      extends Location("Teller & Highway 50", "")

  case Western extends Location("Western", "Colorado & Ohio")

  case DenverAndHighwayOneThirtyFive
      extends Location("Denver & Highway 135", "(City Market)")

  case SpencerAndHighwayOneThirtyFive
      extends Location("Spencer & Highway 135", "(Walmart)")

  case TallTexan extends Location("TallTexan", "(Flag Stop)")

  case OhioCreek extends Location("OhioCreek", "(Flag Stop)")

  case Almont extends Location("Almont", "(Flag Stop)")

  case CBSouth extends Location("CB South", "(Red Mtn Park)")

  case Riverland extends Location("Riverland", "(Flag Stop)")

  case BrushCreek extends Location("Brush Creek", "(Flag Stop)")

  case Riverbend extends Location("Riverbend", "(Flag Stop)")

  case RecCenter extends Location("Rec Center")

  val elementName: String =
    name
      .map((letter: Char) =>
        if (letter.isLetter) letter.toString else "_",
      )
      .mkString

  def matches(
    other: Location,
  ): Boolean =
    this == other || ( // Treat all 3 Four-way stops as equivalent
      (this == FourWayUphill || this == FourwayGunnison) &&
        (other == FourWayUphill || other == FourwayGunnison)
    )

}

object Location {
  val locationsAndIdxs = Location.values.zipWithIndex
  def encodeLocation(
    location: Location,
  ): Int =
    locationsAndIdxs
      .find(_._1 == location)
      .getOrElse(throw new Exception("encodeLocationFailure"))
      ._2

  def decodeLocation(
    locationIdx: Int,
  ): Location =
    locationsAndIdxs
      .find(_._2 == locationIdx)
      .getOrElse(throw new Exception("decodeLocationFailure"))
      ._1

  implicit val codec: JsonCodec[Location] =
    JsonCodec.int.transform(Location.decodeLocation,
                            Location.encodeLocation,
    )

}
