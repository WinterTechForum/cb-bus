package crestedbutte.routes

import crestedbutte.{
  BusSchedule,
  BusScheduleAtStop,
  ComponentName,
  Location,
  NamedRoute,
}
import com.billding.time.MinuteDuration.toMinuteDuration

/*
  Every 60 minutes from 8:25 AM to 10:25 PM
 *From 11:00 p.m. until midnight, a condo bus will run from Mountaineer Square
     to a designated stop on a condo route, on demand only.

  Stop	              Times	First Bus	Last Bus
  Mountaineer Square	:25	  8:25 AM	  10:25 PM
  Whestone	          :26	  8:26 AM	  10:26 PM
  Columbine Condo	    :28	  8:28 AM	  10:28 PM
  Cinnamon Mtn/Gothic	:30	  8:30 AM	  10:30 PM
  Mt. CB Town Hall	  :30	  8:30 AM	  10:30 PM
  Upper Paradise Road :31	  8:31 AM	  10:31 PM
  Lower Paradise Road	:32	  8:32 AM	  8:32 AM
  Eagles Nest Condos	:35	  8:35 PM	  10:35 PM
 */
object ColumbineLoop
    extends NamedRoute(
      ComponentName("Columbine Loop"),
      RouteWithTimes.schedTyped(
        Location.MountaineerSquare,
        _.plus(Location.Whetstone, 1.minutes)
          .plus(Location.ColumbineCondo, 2.minutes)
          .plus(Location.CinnamonMtn, 2.minutes)
          .plus(Location.MtCbTownHall, 0.minutes)
          .plus(Location.UpperParadiseRoad, 1.minutes)
          .plus(Location.LowerParadiseRoad, 1.minutes)
          .plus(Location.EaglesNestCondos, 3.minutes),
        BusSchedule("08:25", "22:25", 60.minutes),
      ),
    )
