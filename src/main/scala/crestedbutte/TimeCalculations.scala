package crestedbutte

import com.billding.time.WallTime
import crestedbutte.laminar.LocationTimeDirection
import crestedbutte.routes.{RtaNorthbound, RtaSouthbound}

object TimeCalculations {
  def updateSegmentFromArbitrarySelection(
    ltd: LocationTimeDirection,
    plan: Plan,
  ) = {
    val other =
      if (ltd.routeSegment.start.l == ltd.locationWithTime.l)
        ltd.routeSegment.end.l
      else if (ltd.routeSegment.end.l == ltd.locationWithTime.l)
        ltd.routeSegment.start.l
      else
        throw new RuntimeException("WTF")
    println("ltd.routeSegment.route: " + ltd.routeSegment.route)
    val routeWithTimes =
      ltd.routeSegment.route match
        case RtaSouthbound.componentName =>
          RtaSouthbound.normalRouteWithTimes
        case RtaNorthbound.componentName =>
          RtaNorthbound.normalRouteWithTimes
        case other =>
          throw new IllegalArgumentException(
            "Route not supported: " + other,
          )
    val newRouteLegThatShouldBeUsedForUpdatingOtherStop =
      routeWithTimes.legs
        .find(routeLeg =>
          routeLeg.stops.contains(ltd.locationWithTime),
        )
        .getOrElse(throw new IllegalStateException("boof"))
    val newOtherValue =
      newRouteLegThatShouldBeUsedForUpdatingOtherStop.stops
        .find(lwt => lwt.l == other)
        .getOrElse(throw new IllegalStateException("doof"))
    val res =
      plan.copy(l = plan.l.map { routeSegment =>
        routeSegment
          .updateTimeAtLocation(
            ltd.locationWithTime,
            ltd.routeSegment.start.t,
            ltd.routeSegment.end.t,
          )
          .updateTimeAtLocation(
            newOtherValue,
            ltd.routeSegment.start.t,
            ltd.routeSegment.end.t,
          )
      })
    res
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
