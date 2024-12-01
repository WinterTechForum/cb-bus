package crestedbutte

import com.billding.time.WallTime
import crestedbutte.laminar.{LocationTimeDirection, SelectedSegmentPiece}
import crestedbutte.routes.RtaSouthbound
import zio.ZIO
import zio.Clock

import java.time.format.DateTimeFormatter
import java.time.{DateTimeException, OffsetDateTime}

object TimeCalculations {
  def updateSegmentFromArbitrarySelection(

                                           ltd: LocationTimeDirection,
                                           plan: Plan
                                         ) = {
      println("We are updating: " + ltd.locationWithTime.l)
      val other =
        if (ltd.routeSegment.start.l == ltd.locationWithTime.l)
          ltd.routeSegment.end.l
        else if (ltd.routeSegment.end.l == ltd.locationWithTime.l)
          ltd.routeSegment.start.l
        else
          throw new RuntimeException("WTF")
      val newRouteLegThatShouldBeUsedForUpdatingOtherStop = RtaSouthbound.normalRouteWithTimes.legs.find(routeLeg => routeLeg.stops.contains(ltd.locationWithTime))
      println(newRouteLegThatShouldBeUsedForUpdatingOtherStop)
      println("We should *also* update: " + other + " on route: " + ltd.routeSegment.route)
      plan.copy(l = plan.l.map {
        // TODO BUG - does not update end location
        routeSegment =>
          routeSegment.updateTimeAtLocation(
            ltd.locationWithTime,
            ltd.routeSegment.start.t,
            ltd.routeSegment.end.t,
          )
      })
  }

  def getUpcomingArrivalInfo(
    departureTimeAtStop: WallTime,
    stops: BusScheduleAtStop,
    now: WallTime,
  ): UpcomingArrivalInfo =
    nextBusArrivalTime(stops.times, departureTimeAtStop)
      .map(nextArrivalTime =>
        UpcomingArrivalInfo(
          stops.location,
          StopTimeInfo(
            nextArrivalTime,
            nextArrivalTime // TODO Return an optional duration based on whether the bus is still in the future
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

  // TODO Shouldn't be part of this class
  def catchableBus(
                    now: WallTime,
                    goal: WallTime,
                  ) =
    goal.localTime
      .isAfter(now.localTime) ||
      goal.localTime
        .equals(now.localTime)

}
