package crestedbutte

import com.billding.time.{MinuteDuration, WallTime}

case class BusScheduleAtStop(
  location: Location,
  times: Seq[WallTime]) {

  def timesDelayedBy(
    busDuration: MinuteDuration,
    locationIn: Location,
  ) = BusScheduleAtStop(locationIn, times.map(_.plus(busDuration)))

  def delayedBy(
    busDuration: MinuteDuration,
  ) = BusScheduleAtStop(location, times.map(_.plus(busDuration)))

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

object BusScheduleAtStop {

  def apply(
    location: Location,
    scheduleAtStop: BusSchedule,
  ): BusScheduleAtStop =
    BusScheduleAtStop(location, scheduleAtStop.stopTimes)

  def combine(
    schedule1: BusScheduleAtStop,
    schedule2: BusScheduleAtStop,
  ): BusScheduleAtStop =
    if (schedule1.location != schedule2.location)
      throw new RuntimeException("Blah")
    else
      BusScheduleAtStop(
        schedule1.location,
//        (schedule1.times ++ schedule2.times).sortBy(_.toString),
        (schedule1.times ++ schedule2.times).sorted,
      ) // TODO Ensure sorted times

}
