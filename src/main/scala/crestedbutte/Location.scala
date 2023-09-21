package crestedbutte

enum Location(
  val name: String,
  val altName: String = "",
  val gpsCoordinates: Option[GpsCoordinates] = None) {

  case blah extends Location("blah")
  case OldTownHall
      extends Location("Old Town Hall",
                       "(Mallardi Theatre)",
                       Some(GpsCoordinates(38.869538, -106.987547)),
      )

  case Clarks
      extends Location("6th/Belleview",
                       "(Clarks Grocery)",
                       Some(GpsCoordinates(38.866970, -106.981499)),
      )

  case FourWayUphill
      extends Location(
        "4-way",
        "(To Mountain)",
        Some(GpsCoordinates(38.870355, -106.980905)),
      ) // gps
  case TeocalliUphill
      extends Location(
        "Teocalli",
        "(To Mountain)",
        Some(GpsCoordinates(38.872718, -106.980830)),
      ) //
  case MountaineerSquare
      extends Location(
        "Mountaineer Square",
        "(CBMR)",
        Some(GpsCoordinates(38.900902, -106.966650)),
      ) //  // This is rough. Maps seems to be off...
  case TeocalliDownhill
      extends Location(
        "Teocalli",
        "(To Downtown)",
        Some(GpsCoordinates(38.872726, -106.981037)),
      ) //
  case FourwayDownhill
      extends Location(
        "4-way",
        "(To Downtown)",
        Some(GpsCoordinates(38.869944, -106.981503)),
      ) //

  case FourwayGunnison
      extends Location(
        "4-way",
        "(To Gunnison)",
      ) //

  // Condo loop entries
  case ThreeSeasons extends Location("Three Seasons")

  case MountainSunrise extends Location("Mountain Sunrise")

  case UpperChateaux extends Location("Upper Chateaux")

  case LowerChateaux extends Location("Lower Chateaux / Marcellina")

  // RTA stops.
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

  // Southbound
  case RecCenter extends Location("Rec Center")

  // END RTA

  // BEGIN Columbine loop stops
  case Whetstone extends Location("Whetstone")

  case ColumbineCondo extends Location("ColumbineCondo")

  case CinnamonMtn extends Location("Cinnamon Mtn / Gothic")

  case MtCbTownHall extends Location("Mt CB Town Hall")

  case UpperParadiseRoad extends Location("Upper Paradise Road")

  case LowerParadiseRoad extends Location("Lower Paradise Road")

  case EaglesNestCondos extends Location("Eagles Nest Condos")

  // END Columbine loop stops
  // BEGIN Crystal/Castle stops
  case Pitchfork extends Location("Pitchfork")

  case CrystalRoad extends Location("Crystal Road")

  case CastleRoad extends Location("Castle Road")

  case WoodCreekMountainEdge
      extends Location("Wood Creek / Mountain Edge")

  case HunterHillTimberline
      extends Location("Hunter Hill / Timberline")
  // END Crystal/Castle stops

  // BEGIN Snodgrass Shuttle Stops

  case CinnamonMtnGothicToSnodgrass
      extends Location("Cinnamon Mtn/Gothic to Snodgrass")

  case GothicWintersetTosnodgrass
      extends Location("Gothic/Winterset to Snodgrass")

  case SnodgrassTrailhead extends Location("Snodgrass Trailhead")

  case GothicWintersetToMountaineerSquare
      extends Location("Gothic/Winterset to Mountaineer Square")

  case MtCBTownHallToMountaineerSquare
      extends Location("Mt CB Town Hall to Mountaineer Square")

  case ParadiseRoad extends Location("Paradise Road")

  case ThePlaza extends Location("The Plaza")

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
      (this == FourWayUphill || this == FourwayGunnison || this == FourwayDownhill) &&
        (other == FourWayUphill || other == FourwayGunnison || other == FourwayDownhill)
    )

}
