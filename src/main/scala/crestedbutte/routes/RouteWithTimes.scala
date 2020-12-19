package crestedbutte.routes

import crestedbutte.{
  BusScheduleAtStop,
  Location,
  LocationWithTime,
  RouteLeg,
}
import com.billding.time.BusDuration

// todo I need to start building this out of RouteLegs.
//    If I correctly construct those, the express issue should go away.
case class RouteWithTimes(
  allStops: Seq[BusScheduleAtStop],
) {

  def routeLeg(
    index: Int,
  ): RouteLeg =
    RouteLeg(
      allStops
        .map(
          stop =>
            LocationWithTime(
              stop.location,
              stop.times.toList(index),
            ),
        ),
    )

  def combinedWith(
    routeWithTimes: RouteWithTimes,
  ): RouteWithTimes =
    new RouteWithTimes(
      (allStops ++: routeWithTimes.allStops)
        .foldLeft(Seq[BusScheduleAtStop]()) {
          case (stopsAcc: Seq[BusScheduleAtStop], nextStop) =>
            val combinedStop =
              stopsAcc.find(_.location == nextStop.location).map {
                existingStop =>
                  BusScheduleAtStop.combine(existingStop, nextStop)
              }
            if (combinedStop.isDefined) {
              val (beforeStop, replacedStop :: afterStop) =
                stopsAcc.splitAt(
                  stopsAcc.indexWhere(
                    _.location == nextStop.location,
                  ),
                )
              beforeStop :+ combinedStop.get :++ afterStop
            }
            else {
              stopsAcc :+ nextStop
            }
        },
    )
}

object RouteWithTimes {

  // TODO transition to this
  def applyNew(
    legs: Seq[RouteLeg],
  ): RouteWithTimes =
    RouteWithTimes(
      legs.foldLeft(Seq[BusScheduleAtStop]()) {
        case (acc, leg) =>
          leg.stops.foldLeft(acc) {
            case (innerAcc, stop) => {
              if (innerAcc.exists(_.location == stop.location))
                innerAcc.map {
                  case BusScheduleAtStop(location, times)
                      if (location == stop.location) =>
                    BusScheduleAtStop(location, times :+ stop.busTime)
                  case other => other
                }
              else
                innerAcc :+ BusScheduleAtStop(stop.location,
                                              Seq(stop.busTime))
            }
          }
      },
    )

  def apply(
    originStops: BusScheduleAtStop,
    locationsWithDelays: Seq[(Location.Value, BusDuration)],
  ) =
    new RouteWithTimes(
      locationsWithDelays
        .foldLeft(Seq(originStops)) {
          case (stopsSoFar, currentStop) =>
            stopsSoFar :+ stopsSoFar.last
              .delayedBy(currentStop._2)
              .at(currentStop._1)
        },
    )

}
