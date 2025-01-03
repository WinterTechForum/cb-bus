package crestedbutte.routes

import com.billding.time.MinuteDuration.toMinuteDuration
import com.billding.time.WallTime
import crestedbutte.*

object RtaSouthbound {
  val componentName = RouteName("Rta Southbound")

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
      RtaSouthbound.componentName,
      Location.MountaineerSquare,
      constructNormalRoute,
      "6:40 AM",
      "7:10 AM",
      "7:40 AM",
      "7:55 AM",
      "8:10 AM",
      "8:25 AM",
      "8:45 AM",
      "9:15 AM",
      "9:45 AM",
      "10:00 AM",
      "10:15 AM",
      "10:30 AM",
      "10:50 AM",
      "11:20 AM",
      "11:50 AM",
      "12:20", // TODO Fix bad parsing of "12:xx" PM times
      "12:55",
      "01:25 PM",
      "01:55 PM",
      "02:10 PM",
      "02:40 PM",
      "03:00 PM",
      "03:30 PM",
      "03:45 PM",
      "04:15 PM",
      "04:30 PM",
      "04:45 PM",
      "05:05 PM",
      "05:35 PM",
      "05:50 PM",
      "06:20 PM",
      "06:50 PM",
      "07:20 PM",
      "07:55 PM",
      "08:25 PM",
      "08:55 PM",
      "09:25 PM",
      "10:00 PM",
      "10:30 PM",
      "11:00 PM",
      "11:30 PM",
    )

  def terminatingRoute(
    busTime: WallTime,
  ): Boolean = busTime.minutes == WallTime("5:35 PM").minutes

  def constructNormalRoute(
    routeLeg: RouteLeg,
  ): RouteLeg = {
    val shortRoute =
      routeLeg
        .plus(Location.FourwayGunnison, 8.minutes)
        .plus(Location.Riverbend, 2.minutes)
        .plus(Location.BrushCreek, 1.minutes)
        .plus(Location.Riverland, 1.minutes)
        .plus(Location.CBSouth, 8.minutes)
        .plus(Location.Almont, 14.minutes)
        .plus(Location.OhioCreek, 8.minutes)
        .plus(Location.TallTexan, 1.minutes)
    val basicRoute =
      if (terminatingRoute(routeLeg.stops.head.t))
        shortRoute
      else
        shortRoute
          .plus(Location.RecCenter, 3.minutes)
          .plus(Location.GunnisonLibrary, 3.minutes)
          .plus(Location.GunnisonCommunitySchools, 1.minutes)
          .plus(Location.EleventhAndVirginia, 2.minutes)
          .plus(Location.Safeway, 2.minutes)
          .plus(Location.TellerAndHighwayFifty, 3.minutes)
          .plus(Location.Western, 2.minutes)

    // late buses actually terminate at the community school. The others loop through Gunni
    // TODO Is this still true?
    if (
      routeLeg.stops.head.t.isBefore(
        WallTime("22:00"),
      ) // && !terminatingRoute(routeLeg.stops.head.t)
    )
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
