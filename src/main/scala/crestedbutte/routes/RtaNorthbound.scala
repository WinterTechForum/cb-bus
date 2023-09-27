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
      "05:56",
      "06:26",
      "06:56",
      "07:26",
      "08:01",
      "08:31",
      "09:01",
      "09:31",
      "10:06",
      "10:36",
      "11:06",
      "11:36",
      "12:11",
      "12:46",
      "13:26",
      "14:16",
      "14:51",
      "15:31",
      "15:56",
      "16:16",
      "17:01",
      "17:31",
      "18:01",
      "19:01",
      "20:06",
      "21:06",
      "22:11",
    )

  val fullSchedule = NamedRoute(
    ComponentName("Rta Northbound"),
    normalRouteWithTimes,
//      .combinedWith(expressRouteWithTimes),
  )

}
