package crestedbutte.routes

import crestedbutte.{
  BusSchedule,
  BusScheduleAtStop,
  Location,
  RouteName,
}
import com.billding.time.MinuteDuration.toMinuteDuration
import crestedbutte.laminar.NamedRoute

/*
  Regular Winter Schedule: November 27th through April 12th

  Every 15 Minutes from 7:10 AM to Midnight

                        Stop	Times	            First Bus	  Last Bus
  Old Town Hall 	      :10, :25, :40, :55	    7:10 AM	    11:40 PM
  6th and Belleview	    :59, :14, :29, :44	    7:14 AM	    11:44 PM
  4-Way Stop	          :00, :15, :30, :45 	    7:15 AM	    11:45 PM
  Teocalli	            :01, :16, :31, :46	    7:16 AM	    11:46 PM
  Mountaineer Square	  :00, :15, :30, :45	    7:30 AM 	  12:00 AM
 */
object TownShuttleTimes
    extends NamedRoute(
      RouteName("Town Loop"),
      RouteWithTimes.schedTyped(
        Location.OldTownHall,
        _.plus(Location.Clarks, 4.minutes)
          .plus(Location.FourWayUphill, 1.minutes)
          .plus(Location.TeocalliUphill, 1.minutes)
          .plus(Location.MountaineerSquare, 14.minutes)
          .plus(Location.TeocalliDownhill, 6.minutes)
          .plus(Location.FourwayDownhill, 1.minutes),
//        BusSchedule("07:10", "23:40", 15.minutes), // Summer/Winter
        BusSchedule("07:35", "22:55", 40.minutes), // Spring/Fall
      ),
    )
