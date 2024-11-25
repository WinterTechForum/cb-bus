package crestedbutte

import com.billding.time.{MinuteDuration, WallTime}

case class BusScheduleAtStop(
  location: Location,
  times: Seq[WallTime],
  routeName: RouteName) {
  val locationsWithTimes =
    times.map(t => LocationWithTime(location, t))

}

object BusScheduleAtStop:
  def apply(
    location: Location,
    scheduleAtStop: BusSchedule,
    routeName: RouteName,
  ): BusScheduleAtStop =
    BusScheduleAtStop(location, scheduleAtStop.stopTimes, routeName)
