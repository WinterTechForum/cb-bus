package crestedbutte.routes

import com.billding.time.MinuteDuration.toMinuteDuration
import crestedbutte.{
  BusSchedule,
  BusScheduleAtStop,
  Location,
  NamedRoute,
  RouteLeg,
  RouteName,
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
      .plus(Location.CBSouth, 16.minutes)
      .plus(Location.Riverland, 6.minutes)
      .plus(Location.BrushCreek, 1.minutes)
      .plus(Location.Riverbend, 1.minutes)
      .plus(Location.FourWayUphill, 5.minutes)
      .plus(Location.MountaineerSquare, 8.minutes)

  val normalRouteWithTimes =
    RouteWithTimes.sched(
      RouteName("Rta Northbound"),
      Location.RecCenter,
      constructNormalRoute,
      "5:21 AM",
      "5:51 AM",
      "6:21 AM",
      "6:36 AM",
      "6:51 AM",
      "7:06 AM",
      "7:26 AM",
      "7:56 AM",
      "8:26 AM",
      "8:41 AM",
      "8:56 AM",
      "9:11 AM",
      "9:31 AM",
      "10:01 AM",
      "10:31 AM",
      "11:01 AM",
      "11:36 AM",
      "12:06", // TODO Fix bad parsing of "12:xx" PM times
      "12:36",
      "12:51",
      "1:21 PM",
      "1:41 PM",
      "2:11 PM",
      "2:26 PM",
      "2:56 PM",
      "3:11 PM",
      "3:26 PM",
      "3:46 PM",
      "4:16 PM",
      "4:31 PM",
      "5:01 PM",
      "5:31 PM",
      "6:01 PM",
      "6:36 PM",
      "7:06 PM",
      "7:36 PM",
      "8:06 PM",
      "8:41 PM",
      "9:11 PM",
      "9:41 PM",
      "10:11 PM",
    )

  val fullSchedule = NamedRoute(
    RouteName("Rta Northbound"),
    normalRouteWithTimes,
//      .combinedWith(expressRouteWithTimes),
  )

}
