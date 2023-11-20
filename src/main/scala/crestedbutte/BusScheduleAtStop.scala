package crestedbutte

import com.billding.time.{MinuteDuration, WallTime}

case class BusScheduleAtStop(
  location: Location,
  times: Seq[WallTime]) {

  def at(
    locationIn: Location,
  ) = BusScheduleAtStop(locationIn, times)

  def scheduleAfter(
    busTime: WallTime,
  ) =
    BusScheduleAtStop(
      location,
      times.dropWhile(!TimeCalculations.catchableBus(busTime, _)),
    )

}

object BusScheduleAtStop:
  def apply(
    location: Location,
    scheduleAtStop: BusSchedule,
  ): BusScheduleAtStop =
    BusScheduleAtStop(location, scheduleAtStop.stopTimes)
