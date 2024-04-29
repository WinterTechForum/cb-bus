package crestedbutte

import com.billding.time.WallTime
import zio.ZIO
import zio.Clock

import java.time.format.DateTimeFormatter
import java.time.{DateTimeException, OffsetDateTime}

object TimeCalculations {

  // TODO Shouldn't be part of this class
  def catchableBus(
    now: WallTime,
    goal: WallTime,
  ) =
    goal.localTime
      .isAfter(now.localTime) ||
      goal.localTime
        .equals(now.localTime)

  def getUpcomingArrivalInfo(
    stops: BusScheduleAtStop,
    now: WallTime,
  ): UpcomingArrivalInfo =
    nextBusArrivalTime(stops.times, now)
      .map(nextArrivalTime =>
        UpcomingArrivalInfo(
          stops.location,
          StopTimeInfo(
            nextArrivalTime,
            nextArrivalTime
              .between(now),
          ),
        ),
      )
      .getOrElse(
        UpcomingArrivalInfo(
          stops.location,
          LateNightRecommendation("Late Shuttle"),
        ),
      )

  private def nextBusArrivalTime(
                                  timesAtStop: Seq[WallTime],
                                  now: WallTime,
                                ): Option[WallTime] =
    timesAtStop
      .find(stopTime => TimeCalculations.catchableBus(now, stopTime))
      .filter(_ => now.isLikelyEarlyMorningRatherThanLateNight)

}
