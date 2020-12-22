package crestedbutte.routes

import com.billding.time.BusDuration.toBusDuration
import com.billding.time.BusTime
import crestedbutte._

object RtaSouthbound {

  def constructExpressRoute(
    routeLeg: RouteLeg,
  ): RouteLeg = {
    val basicRoute =
      routeLeg
        .plus(Location.FourwayGunnison, 8.minutes)
        .plus(Location.Riverbend, 3.minutes)
        .plus(Location.BrushCreek, 1.minutes)
        .plus(Location.Riverland, 1.minutes)
        .plus(Location.Almont, 14.minutes)
        .plus(Location.OhioCreek, 8.minutes)
        .plus(Location.TallTexan, 1.minutes)
        .plus(Location.RecCenter, 3.minutes)
        .plus(Location.GunnisonCommunitySchools, 4.minutes)

    // late buses actually terminate at the community school. The others loop through Gunni
    if (routeLeg.stops.head.busTime.isBefore(BusTime("22:00")))
      // Deviating from the PDFs, for usability's sake!
      basicRoute
        .plus(Location.EleventhAndVirginia, 2.minutes)
        .plus(Location.Safeway, 4.minutes)
        .plus(Location.TellerAndHighwayFifty, 2.minutes)
        .plus(Location.Western, 2.minutes)
        .plus(Location.DenverAndHighwayOneThirtyFive, 3.minutes)
    else
      basicRoute
  }

  val expressRouteWithTimes =
    RouteWithTimes.sched(
      Location.MountaineerSquare,
      constructExpressRoute,
      "15:15",
      "15:45",
      "16:15",
      "16:45",
      "17:15",
      "17:30",
      "17:45",
    )

  val normalRouteWithTimes =
    RouteWithTimes.sched(
      Location.MountaineerSquare,
      constructNormalRoute,
      "06:40",
      "07:10",
      "07:40",
      "08:10",
      "08:40",
      "09:10",
      "09:40",
      "10:15",
      "10:45",
      "11:15",
      "11:45",
      "12:25",
      "13:25",
      "14:15",
      "14:40",
      "18:15",
      "19:15",
      "20:20",
      "21:20",
      "22:25",
      "23:25",
    )

  def constructNormalRoute(
    routeLeg: RouteLeg,
  ): RouteLeg = {
    val basicRoute =
      routeLeg
        .plus(Location.FourwayGunnison, 8.minutes)
        .plus(Location.Riverbend, 3.minutes)
        .plus(Location.BrushCreek, 1.minutes)
        .plus(Location.Riverland, 1.minutes)
        .plus(Location.CBSouth, 7.minutes)
        .plus(Location.Almont, 14.minutes)
        .plus(Location.OhioCreek, 8.minutes)
        .plus(Location.TallTexan, 1.minutes)
        .plus(Location.RecCenter, 3.minutes)
        .plus(Location.GunnisonCommunitySchools, 4.minutes)

    // late buses actually terminate at the community school. The others loop through Gunni
    if (routeLeg.stops.head.busTime.isBefore(BusTime("22:00")))
      // Deviating from the PDFs, for usability's sake!
      basicRoute
        .plus(Location.EleventhAndVirginia, 2.minutes)
        .plus(Location.Safeway, 4.minutes)
        .plus(Location.TellerAndHighwayFifty, 2.minutes)
        .plus(Location.Western, 2.minutes)
        .plus(Location.DenverAndHighwayOneThirtyFive, 3.minutes)
    else
      basicRoute
  }

  val fullSchedule = NamedRoute(
    RouteName("Rta Southbound"),
    normalRouteWithTimes.combinedWith(expressRouteWithTimes),
  )

}
