package crestedbutte

import com.billding.time.BusTime
import crestedbutte.laminar.NamedRoute
import zio.{Has, ZIO}
import zio.clock.Clock

import java.time.{DateTimeException, OffsetDateTime}

object TimeCalculations {

  def nextBusArrivalTime(
    timesAtStop: Seq[BusTime],
    now: BusTime,
  ): Option[BusTime] =
    timesAtStop
      .find(stopTime => BusTime.catchableBus(now, stopTime))
      .filter(_ => now.isLikelyEarlyMorningRatherThanLateNight)

  def getUpcomingArrivalInfo(
    stops: BusScheduleAtStop,
    now: BusTime,
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
    now: BusTime,
    busRoute: NamedRoute,
  ): Seq[UpcomingArrivalInfo] =
    busRoute.routeWithTimes.allStops.map(
      scheduleAtStop => getUpcomingArrivalInfo(scheduleAtStop, now),
    )

  def calculateUpcomingArrivalWithFullScheduleAtAllStops(
    now: BusTime,
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
      localTime = new BusTime(now.toLocalTime)
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
      localTime = new BusTime(now.toLocalTime)
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
    localTime: BusTime,
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
