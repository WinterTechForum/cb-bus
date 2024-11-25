package crestedbutte.routes

import com.billding.time.MinuteDuration.toMinuteDuration
import crestedbutte.{
  BusSchedule,
  BusScheduleAtStop,
  Location,
  NamedRoute,
  RouteName,
}

object SnodgrassShuttle
/*
    extends NamedRoute(
      RouteName("Snodgrass Shuttle"),
      RouteWithTimes.schedTyped(
        RouteName("Snodgrass Shuttle"),
        Location.MountaineerSquare,
        _.plus(Location.CinnamonMtnGothicToSnodgrass, 1.minutes)
          .plus(Location.GothicWintersetTosnodgrass, 2.minutes)
          .plus(Location.SnodgrassTrailhead, 2.minutes)
          .plus(Location.GothicWintersetToMountaineerSquare,
                2.minutes,
          )
          .plus(Location.MtCBTownHallToMountaineerSquare, 1.minutes)
          .plus(Location.ParadiseRoad, 1.minutes),
        BusSchedule("07:55", "21:55", 60.minutes),
      ),
    )
 */