package crestedbutte.routes

import com.billding.time.MinuteDuration.toMinuteDuration
import crestedbutte.{
  BusSchedule,
  BusScheduleAtStop,
  ComponentName,
  Location,
  NamedRoute,
  RouteLeg,
}

object RtaNorthbound {

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
      Location.RecCenter,
      constructNormalRoute,
      "05:21",
      "5:51",
      "6:21",
      "6:36",
      "6:51",
      "7:06",
      "7:26",
      "7:56",
      "8:26",
      "8:41",
      "8:56",
      "9:11",
      "9:31",
      "10:01",
      "10:31",
      "11:01",
      "11:36",
      "12:06",
      "12:36",
      "12:51",
      "13:21",
      "13:41",
      "14:11",
      "14:26",
      "14:56",
      "15:11",
      "15:26",
      "15:46",
      "16:16",
      "16:31",
      "17:01",
      "17:31",
      "18:01",
      "18:36",
      "19:06",
      "19:36",
      "20:06",
      "20:41",
      "21:11",
      "21:41",
      "22:11",
    )

  val fullSchedule = NamedRoute(
    ComponentName("Rta Northbound"),
    normalRouteWithTimes,
//      .combinedWith(expressRouteWithTimes),
  )

}
