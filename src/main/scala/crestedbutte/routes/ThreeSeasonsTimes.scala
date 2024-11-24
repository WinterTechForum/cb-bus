package crestedbutte.routes

import crestedbutte.{
  BusSchedule,
  BusScheduleAtStop,
  RouteName,
  Location,
  NamedRoute,
}
import com.billding.time.MinuteDuration.toMinuteDuration

/*

  // TODO How should I indicate when it arrives back at Mountaineer Square?
  //    Currently, it's hard to determine what buses you could catch from the Square.
 */
object ThreeSeasonsTimes
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
