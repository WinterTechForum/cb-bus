package crestedbutte

import com.billding.time.{MinuteDuration, WallTime}

case class BusSchedule(
  stopTimes: List[WallTime]) {}

object BusSchedule {

  def apply(
    firstBus: String,
    lastBus: String,
    timeBetweenBuses: MinuteDuration,
  ) = {
    val startTime = WallTime(firstBus)
    val endTime = WallTime(lastBus)

    new BusSchedule(
      LazyList
        .iterate(startTime)(_.plus(timeBetweenBuses))
        .takeWhile(time => time.isBeforeOrEqualTo(endTime))
        .toList,
    )
  }

}
