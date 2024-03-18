package crestedbutte

import com.billding.time.{MinuteDuration, WallTime}
import crestedbutte.TripParamZ.StartingAfter
import crestedbutte.routes.RouteWithTimes
import pprint.PPrinter

import zio.json._

case class TripPlannerError(
  msg: String)

case class LocationWithTime(
                             l: Location,
                             t: WallTime)
    derives JsonCodec

//object LocationWithTime {
//  implicit val codec: JsonCodec[LocationWithTime] = 
//    Location.codec.zip(JsonCodec[WallTime]).transform[LocationWithTime](
//      (str, time) => LocationWithTime(str, time),
//      locationWithTime => (locationWithTime.location, locationWithTime.t)
//    )
//    DeriveJsonCodec.gen[LocationWithTime]
//  
//}

enum TripBoundary:
  case StartingAfter, ArrivingBy

enum TripParamZ {
  case StartingAfter(
    start: Location,
    arrivalTime: WallTime, // TODO rename to earliestDeparture
    routeWithTimes: RouteWithTimes,
    destination: Location)

  case ArrivingBy(
    start: Location,
    arrivalTime: WallTime,
    destination: Location,
    leaveSchedule: RouteWithTimes)

  import math.Ordered.orderingToOrdered // Enables <= comparison for wall times
  def evaluate(
  ): Either[TripPlannerError, RouteLeg] =
    this match
      case s: StartingAfter =>
        s.routeWithTimes.legs
          .find(leg =>
            leg.stops.exists(stop =>
              stop.l
                .matches(
                  s.start,
                ) && stop.t >= s.arrivalTime,
            ),
          )
          .flatMap: routeLeg =>
            (for
              trimmedStart <- routeLeg.trimToStartAt(s.start)
              trimmedEnd   <- trimmedStart.trimToEndAt(s.destination)
            yield trimmedEnd).toOption
          .toRight(
            TripPlannerError(
              "Could not find a return leg after: " + s.arrivalTime.toDumbAmericanString,
            ),
          )
      case b: ArrivingBy =>
        b.leaveSchedule.legs
          .findLast(leg =>
            leg.stops.exists(stop =>
              stop.l
                .matches(
                  b.destination,
                ) && stop.t <= b.arrivalTime,
            ),
          )
          .flatMap(routeLeg =>
            (for
              trimmedStart <- routeLeg.trimToStartAt(b.start)
              trimmedEnd   <- trimmedStart.trimToEndAt(b.destination)
            yield trimmedEnd).toOption,
          )
          .toRight(
            TripPlannerError(
              "Could not find a departing leg arriving by " + b.arrivalTime.toDumbAmericanString,
            ),
          )

}

case class TripParams(
  startLocation: Location,
  destination: Location,
  arrivalTime: WallTime,
  leaveSchedule: RouteWithTimes,
  departureTime: WallTime,
  returningLaunchPoint: Location,
  returnSchedule: RouteWithTimes)

case class Trip(
  leave: RouteLeg,
  returnLeg: RouteLeg)

object TripPlanner {}
