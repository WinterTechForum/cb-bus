package crestedbutte.routes

import com.billding.time.MinuteDuration.toMinuteDuration
import com.billding.time.WallTime
import crestedbutte.*

object RtaSouthbound {

//  def constructExpressRoute(
//    routeLeg: RouteLeg,
//  ): RouteLeg = {
//    val basicRoute =
//      routeLeg
//        .plus(Location.FourwayGunnison, 8.minutes)
//        .plus(Location.Riverbend, 3.minutes)
//        .plus(Location.BrushCreek, 1.minutes)
//        .plus(Location.Riverland, 1.minutes)
//        .plus(Location.Almont, 14.minutes)
//        .plus(Location.OhioCreek, 8.minutes)
//        .plus(Location.TallTexan, 1.minutes)
//        .plus(Location.RecCenter, 3.minutes)
//        .plus(Location.GunnisonCommunitySchools, 4.minutes)
//
//    // late buses actually terminate at the community school. The others loop through Gunni
//    if (routeLeg.stops.head.busTime.isBefore(WallTime("22:00")))
//      // Deviating from the PDFs, for usability's sake!
//      basicRoute
//        .plus(Location.EleventhAndVirginia, 2.minutes)
//        .plus(Location.Safeway, 4.minutes)
//        .plus(Location.TellerAndHighwayFifty, 2.minutes)
//        .plus(Location.Western, 2.minutes)
//        .plus(Location.DenverAndHighwayOneThirtyFive, 3.minutes)
//    else
//      basicRoute
//  }

//  val expressRouteWithTimes =
//    RouteWithTimes.sched(
//      Location.MountaineerSquare,
//      constructExpressRoute,
//      "15:15",
//      "15:45",
//      "16:15",
//      "16:45",
//      "17:15",
//      "17:30",
//      "17:45",
//    )

  val normalRouteWithTimes =
    RouteWithTimes.sched(
      Location.MountaineerSquare,
      constructNormalRoute,
      "06:40",
      "07:15",
      "07:45",
      "08:15",
      "08:45",
      "09:20",
      "09:50",
      "10:20",
      "10:50",
      "11:25",
      "12:00",
      "12:30",
      "13:00",
      "13:30",
      "14:05",
      "14:45",
      "15:30",
      "16:15",
      "16:45",
      "17:15",
      "17:45",
      "18:15",
      "18:50",
      "19:20",
      "20:20",
      "21:25",
      "22:25",
      "23:30",
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
        .plus(Location.RecCenter, 3.minutes)
        .plus(Location.GunnisonLibrary, 3.minutes)
        .plus(Location.GunnisonCommunitySchools, 1.minutes)
        .plus(Location.EleventhAndVirginia, 2.minutes)
        .plus(Location.Safeway, 2.minutes)
        .plus(Location.TellerAndHighwayFifty, 3.minutes)
        .plus(Location.Western, 2.minutes)

    // late buses actually terminate at the community school. The others loop through Gunni
    if (routeLeg.stops.head.busTime.isBefore(WallTime("22:00")))
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
    normalRouteWithTimes,
//      .combinedWith(expressRouteWithTimes),
  )

}
