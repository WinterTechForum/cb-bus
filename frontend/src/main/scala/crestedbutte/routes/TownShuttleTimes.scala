package crestedbutte.routes

import crestedbutte.{
  BusSchedule,
  BusScheduleAtStop,
  Location,
  NamedRoute,
  RouteName,
}
import com.billding.time.MinuteDuration.toMinuteDuration

object TownShuttleTimes
/*
    extends NamedRoute(
      RouteName("Town Loop"),
      RouteWithTimes.schedTyped(
        RouteName("Town Loop"),
        Location.OldTownHall,
        _.plus(Location.Clarks, 4.minutes)
          .plus(Location.FourWayUphill, 1.minutes)
          .plus(Location.TeocalliUphill, 1.minutes)
          .plus(Location.MountaineerSquare, 25.minutes)
          .plus(Location.TeocalliDownhill, 6.minutes)
          .plus(Location.FourwayDownhill, 1.minutes),
        BusSchedule("07:10", "23:40", 15.minutes), // Summer/Winter
//        BusSchedule("07:35", "22:55", 40.minutes), // Spring/Fall
      ),
    )
 */
