package crestedbutte.routes

import com.billding.time.MinuteDuration.toMinuteDuration
import crestedbutte.{
  BusSchedule,
  BusScheduleAtStop,
  RouteName,
  Location,
  NamedRoute,
  RouteLeg,
}

object RtaNorthbound {
  val componentName = RouteName("Rta Northbound")

//  def constructExpressRoute(
//    routeLeg: RouteLeg,
//  ): RouteLeg =
//    routeLeg
//      .plus(Location.EleventhAndVirginia, 2.minutes)
//      .plus(Location.Safeway, 4.minutes)
//      .plus(Location.TellerAndHighwayFifty, 2.minutes)
//      .plus(Location.Western, 2.minutes)
//      .plus(Location.DenverAndHighwayOneThirtyFive, 3.minutes)
//      .plus(Location.SpencerAndHighwayOneThirtyFive, 2.minutes)
//      .plus(Location.TallTexan, 2.minutes)
//      .plus(Location.OhioCreek, 1.minutes)
//      .plus(Location.Almont, 7.minutes)
//      .plus(Location.Riverland, 16.minutes)
//      .plus(Location.BrushCreek, 1.minutes)
//      .plus(Location.Riverbend, 1.minutes)
//      .plus(Location.FourWayUphill, 3.minutes)
//      .plus(Location.MountaineerSquare, 10.minutes)

//  val expressRouteWithTimes =
//    RouteWithTimes.sched(
//      Location.GunnisonCommunitySchools,
//      constructExpressRoute,
//      "06:30",
//      "06:45",
//      "07:00",
//      "07:30",
//      "08:00",
//      "08:30",
//    )

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
      .plus(Location.CBSouth, 15.minutes)
      .plus(Location.Riverland, 6.minutes)
      .plus(Location.BrushCreek, 1.minutes)
      .plus(Location.Riverbend, 1.minutes)
      .plus(Location.FourWayUphill, 4.minutes)
      .plus(Location.MountaineerSquare, 8.minutes)

  val normalRouteWithTimes =
    RouteWithTimes.sched(
      RouteName("Rta Northbound"),
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
      "12:06", // TODO Fix bad parsing of "12:xx" PM times
      "12:36",
      "1:06 PM",
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
//      .combinedWith(expressRouteWithTimes),
  )

}
