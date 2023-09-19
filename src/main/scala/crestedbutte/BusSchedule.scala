package crestedbutte

import com.billding.time.{MinuteDuration, WallTime}

case class BusSchedule(
  stopTimes: List[WallTime]) {

  def nextBusArrivalTime(
    now: WallTime,
  ): Option[WallTime] =
    if (now.isLikelyEarlyMorningRatherThanLateNight)
      stopTimes
        .find(stopTime =>
          TimeCalculations.catchableBus(now, stopTime),
        )
    else None
}

object BusSchedule {

  def apply(
    firstBus: String,
    lastBus: String,
    timeBetweenBuses: MinuteDuration,
  ) =
    new BusSchedule(
      List
        .range(
          0L,
          WallTime(firstBus)
            .between(WallTime(lastBus))
            .dividedBy(timeBetweenBuses) + 1,
        ) // TODO Ugh. Nasty +1
        .map(index =>
          WallTime(firstBus)
            .plus(timeBetweenBuses.times(index.toInt)),
        ),
    )

  // Useful for irregular stoptimes
  def apply(
    stopTimeStrings: String*,
  ) =
    new BusSchedule(
      List(stopTimeStrings: _*)
        .map(WallTime(_)),
    )
}
