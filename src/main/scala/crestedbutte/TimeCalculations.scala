package crestedbutte

import com.billding.time.WallTime
import crestedbutte.laminar.NamedRoute
import zio.{Has, ZIO}
import zio.clock.Clock

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

  def nextBusArrivalTime(
    timesAtStop: Seq[WallTime],
    now: WallTime,
  ): Option[WallTime] =
    timesAtStop
      .find(stopTime => TimeCalculations.catchableBus(now, stopTime))
      .filter(_ => now.isLikelyEarlyMorningRatherThanLateNight)

  def getUpcomingArrivalInfo(
    stops: BusScheduleAtStop,
    now: WallTime,
  ): UpcomingArrivalInfo =
    nextBusArrivalTime(stops.times, now)
      .map(
        nextArrivalTime =>
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

  def calculateUpcomingArrivalAtAllStops(
    now: WallTime,
    busRoute: NamedRoute,
  ): Seq[UpcomingArrivalInfo] =
    busRoute.routeWithTimes.allStops.map(
      scheduleAtStop => getUpcomingArrivalInfo(scheduleAtStop, now),
    )

  def calculateUpcomingArrivalWithFullScheduleAtAllStops(
    now: WallTime,
    busRoute: NamedRoute,
  ): Seq[UpcomingArrivalInfoWithFullSchedule] =
    busRoute.routeWithTimes.allStops.map(
      scheduleAtStop =>
        UpcomingArrivalInfoWithFullSchedule(
          getUpcomingArrivalInfo(scheduleAtStop, now),
          scheduleAtStop.scheduleAfter(now),
        ),
    )

  def getUpComingArrivals(
    busRoute: NamedRoute,
  ) =
    for {
      clockProper <- ZIO.access[Clock](_.get)
      now         <- clockProper.currentDateTime
      localTime = WallTime(
        now.toLocalTime.format(
          DateTimeFormatter.ofPattern("HH:mm"),
        ),
      )
    } yield {
      TimeCalculations.calculateUpcomingArrivalAtAllStops(
        localTime,
        busRoute,
      )
    }

  def getUpComingArrivalsWithFullSchedule(
    busRoute: NamedRoute,
  ): ZIO[Clock, DateTimeException, UpcomingArrivalComponentData] =
    for {
      clockProper <- ZIO.access[Clock](_.get)
      now         <- clockProper.currentDateTime
      localTime = WallTime(
        now.toLocalTime.format(
          DateTimeFormatter.ofPattern("HH:mm"),
        ),
      )
    } yield {
      println("in schedule code")
      UpcomingArrivalComponentData(
        TimeCalculations
          .calculateUpcomingArrivalWithFullScheduleAtAllStops(
            localTime,
            busRoute,
          ),
        busRoute.routeName,
      )
    }

  def getUpComingArrivalsWithFullScheduleNonZio(
    localTime: WallTime,
    busRoute: NamedRoute,
  ): UpcomingArrivalComponentData =
    UpcomingArrivalComponentData(
      TimeCalculations
        .calculateUpcomingArrivalWithFullScheduleAtAllStops(
          localTime,
          busRoute,
        ),
      busRoute.routeName,
    )
}
