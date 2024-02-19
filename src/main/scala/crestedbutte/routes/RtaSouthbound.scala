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
      ComponentName("Rta Southbound"),
      Location.MountaineerSquare,
      constructNormalRoute,
      "06:40",
      "7:10",
      "7:40",
      "7:55",
      "8:10",
      "8:25",
      "8:45",
      "9:15",
      "9:45",
      "10:00",
      "10:15",
      "10:30",
      "10:50",
      "11:20",
      "11:50",
      "12:20",
      "12:55",
      "13:25",
      "13:55",
      "14:10",
      "14:40",
      "15:00",
      "15:30",
      "15:45",
      "16:15",
      "16:30",
      "16:45",
      "17:05",
      "17:35",
      "17:50",
      "18:20",
      "18:50",
      "19:20",
      "19:55",
      "20:25",
      "20:55",
      "21:25",
      "22:00",
      "22:30",
      "23:00",
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
    ComponentName("Rta Southbound"),
    normalRouteWithTimes,
//      .combinedWith(expressRouteWithTimes),
  )

}
