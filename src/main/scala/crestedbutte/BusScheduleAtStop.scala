package crestedbutte

import com.billding.time.{MinuteDuration, WallTime}

case class BusScheduleAtStop(
  location: Location,
  times: Seq[WallTime],
  routeName: ComponentName) {

  def at(
    locationIn: Location,
  ) = BusScheduleAtStop(locationIn, times, routeName)

  def scheduleAfter(
    busTime: WallTime,
  ) =
    BusScheduleAtStop(
      location,
      times.dropWhile(!TimeCalculations.catchableBus(busTime, _)),
      routeName,
    )

}

object BusScheduleAtStop:
  def apply(
    location: Location,
    scheduleAtStop: BusSchedule,
    routeName: ComponentName,
  ): BusScheduleAtStop =
    BusScheduleAtStop(location, scheduleAtStop.stopTimes, routeName)
