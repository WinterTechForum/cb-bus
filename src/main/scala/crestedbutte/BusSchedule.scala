package crestedbutte

import com.billding.time.{MinuteDuration, WallTime}

case class BusSchedule(
  stopTimes: List[WallTime]) {}

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

}
