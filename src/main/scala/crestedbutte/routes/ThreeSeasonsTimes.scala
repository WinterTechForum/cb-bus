package crestedbutte.routes

import crestedbutte.{
  BusSchedule,
  BusScheduleAtStop,
  Location,
  NamedRoute,
  RouteName,
}
import com.billding.time.MinuteDuration.toMinuteDuration

object ThreeSeasonsTimes
/*
    extends NamedRoute(
      RouteName("Three Seasons Loop"),
      RouteWithTimes.schedTyped(
        RouteName("Three Seasons Loop"),
        Location.MountaineerSquare,
        _.plus(Location.ThreeSeasons, 1.minutes)
          .plus(Location.MountainSunrise, 1.minutes)
          .plus(Location.UpperChateaux, 0.minutes)
          .plus(Location.LowerChateaux, 1.minutes),
        BusSchedule("08:00", "22:45", 15.minutes),
      ),
    )
 */