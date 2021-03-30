package crestedbutte.routes

import com.billding.time.MinuteDuration.toMinuteDuration
import crestedbutte.laminar.NamedRoute
import crestedbutte.{
  BusSchedule,
  BusScheduleAtStop,
  Location,
  RouteLeg,
  RouteName,
}

object RtaNorthbound {

  def constructExpressRoute(
    routeLeg: RouteLeg,
  ): RouteLeg =
    routeLeg
      .plus(Location.EleventhAndVirginia, 2.minutes)
      .plus(Location.Safeway, 4.minutes)
      .plus(Location.TellerAndHighwayFifty, 2.minutes)
      .plus(Location.Western, 2.minutes)
      .plus(Location.DenverAndHighwayOneThirtyFive, 3.minutes)
      .plus(Location.SpencerAndHighwayOneThirtyFive, 2.minutes)
      .plus(Location.TallTexan, 2.minutes)
      .plus(Location.OhioCreek, 1.minutes)
      .plus(Location.Almont, 7.minutes)
      .plus(Location.Riverland, 16.minutes)
      .plus(Location.BrushCreek, 1.minutes)
      .plus(Location.Riverbend, 1.minutes)
      .plus(Location.FourWayUphill, 3.minutes)
      .plus(Location.MountaineerSquare, 10.minutes)

  val expressRouteWithTimes =
    RouteWithTimes.sched(
      Location.GunnisonCommunitySchools,
      constructExpressRoute,
      "06:30",
      "06:45",
      "07:00",
      "07:30",
      "08:00",
      "08:30",
    )

  def constructNormalRoute(
    routeLeg: RouteLeg,
  ): RouteLeg =
    routeLeg
      .plus(Location.EleventhAndVirginia, 2.minutes)
      .plus(Location.Safeway, 4.minutes)
      .plus(Location.TellerAndHighwayFifty, 2.minutes)
      .plus(Location.Western, 2.minutes)
      .plus(Location.DenverAndHighwayOneThirtyFive, 3.minutes)
      .plus(Location.SpencerAndHighwayOneThirtyFive, 2.minutes)
      .plus(Location.TallTexan, 2.minutes)
      .plus(Location.OhioCreek, 1.minutes)
      .plus(Location.Almont, 7.minutes)
      .plus(Location.CBSouth, 17.minutes)
      .plus(Location.Riverland, 5.minutes)
      .plus(Location.BrushCreek, 1.minutes)
      .plus(Location.Riverbend, 1.minutes)
      .plus(Location.FourWayUphill, 3.minutes)
      .plus(Location.MountaineerSquare, 10.minutes)

  val normalRouteWithTimes =
    RouteWithTimes.sched(
      Location.GunnisonCommunitySchools,
      constructNormalRoute,
      "05:30",
      "06:00",
      "09:00",
      "09:30",
      "10:00",
      "10:30",
      "11:05",
      "11:35",
      "12:05",
      "13:00",
      "13:30",
      "14:15",
      "15:05",
      "15:30",
      "16:00",
      "16:30",
      "17:00",
      "18:00",
      "19:05",
      "20:05",
      "21:10",
      "22:10",
    )

  val fullSchedule = NamedRoute(
    RouteName("Rta Northbound"),
    normalRouteWithTimes.combinedWith(expressRouteWithTimes),
  )

}
