package crestedbutte.routes

import com.billding.time.MinuteDuration.toMinuteDuration
import com.billding.time.WallTime
import crestedbutte.*

object RTA {
  object Southbound {
    val componentName = RouteName("Rta Southbound")

    val normalRouteWithTimes =
      RouteWithTimes.sched(
        Southbound.componentName,
        Location.MountaineerSquare,
        constructNormalRoute,
        "6:40 AM",
        "7:10 AM",
        "7:40 AM",
        "8:15 AM",
        "8:45 AM",
        "9:15 AM",
        "9:45 AM",
        "10:20 AM",
        "10:50 AM",
        "11:20 AM",
        "11:50 AM",
        "12:20", // TODO Fix bad parsing of "12:xx" PM times
        "12:55",
        "01:25 PM",
        "01:55 PM",
        "02:25 PM",
        "03:00 PM",
        "03:30 PM",
        "04:00 PM",
        "04:30 PM",
        "05:05 PM",
        "05:35 PM",
        "06:05 PM",
        "06:35 PM",
        "07:05 PM",
        "07:35 PM",
        "08:10 PM",
        "08:40 PM",
        "09:10 PM",
        "09:40 PM",
        "10:20 PM",
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
    )
  }

  object Northbound {
    val componentName = RouteName("Rta Northbound")

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
        Northbound.componentName,
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
        //
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
    )
  }
}
