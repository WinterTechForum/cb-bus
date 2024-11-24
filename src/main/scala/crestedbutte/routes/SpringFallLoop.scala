package crestedbutte.routes

import crestedbutte.{
  BusSchedule,
  BusScheduleAtStop,
  RouteName,
  Location,
  NamedRoute,
}
import com.billding.time.MinuteDuration.toMinuteDuration

object SpringFallLoop
    extends NamedRoute(
      RouteName("Off-season Loop"),
      RouteWithTimes.schedTyped(
        RouteName("Off-season Loop"),
        Location.OldTownHall,
        _.plus(Location.Clarks, 5.minutes)
          .plus(Location.FourWayUphill, 1.minutes)
          .plus(Location.TeocalliUphill, 1.minutes)
          .plus(Location.WoodCreekMountainEdge, 4.minutes)
          .plus(Location.ThePlaza, 1.minutes)
          .plus(Location.MountaineerSquare, 13.minutes)
          .plus(Location.ThreeSeasons, 1.minutes)
          .plus(Location.UpperChateaux, 1.minutes)
          .plus(Location.LowerChateaux, 0.minutes)
          .plus(Location.Pitchfork, 1.minutes)
          .plus(Location.TeocalliDownhill, 4.minutes),
        BusSchedule("07:35", "22:55", 40.minutes),
      ),
    )
