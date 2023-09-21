package crestedbutte.routes

import com.billding.time.MinuteDuration.toMinuteDuration
import crestedbutte.{
  BusSchedule,
  BusScheduleAtStop,
  Location,
  NamedRoute,
  RouteName,
}

/*
Every 30 minutes from 8:10 AM to 10:40 PM
 *From 11:00 p.m. until midnight, a condo bus will run from Mountaineer Square
 to a designated stop on a condo route, on demand only.

  Stop	                    Times	    First Bus	Last Bus
  Mountaineer Square	      :10, :40	8:10 AM	  10:40 PM
  Pitchfork	                :12, :42	8:12 AM	  10:42 PM
  Crystal Road	            :13, :43	8:13 AM	  10:43 PM
  Castle Road	              :15, :45	8:15 AM	  10:45 PM
  Wood Creek/Mountain Edge	:16, :46	8:16 AM	  10:46 PM
  Hunter Hill/Timberline	  :18, :48	8:18 AM	  10:48 PM

 */
object CrystalCastleShuttle
    extends NamedRoute(
      RouteName("Crystal/Castle"),
      RouteWithTimes.schedTyped(
        Location.MountaineerSquare,
        _.plus(Location.Pitchfork, 2.minutes)
          .plus(Location.CrystalRoad, 1.minutes)
          .plus(Location.CastleRoad, 2.minutes)
          .plus(Location.WoodCreekMountainEdge, 1.minutes)
          .plus(Location.HunterHillTimberline, 2.minutes),
        BusSchedule("08:10", "22:40", 30.minutes),
      ),
    ) {}
