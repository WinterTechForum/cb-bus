package crestedbutte.laminar

import animus.*
import com.billding.time.WallTime
import com.raquo.laminar.api.L.*
import com.raquo.laminar.nodes.ReactiveHtmlElement
import crestedbutte.*
import crestedbutte.NotificationStuff.desiredAlarms
import crestedbutte.dom.BulmaLocal
import crestedbutte.dom.BulmaLocal.UpcomingStops
import crestedbutte.laminar.TouchControls.Swipe
import crestedbutte.pwa.Persistence
import crestedbutte.routes.{CompleteStopList, RTA, RouteWithTimes}
import java.time.format.DateTimeFormatter
import java.time.{Clock, OffsetDateTime}
import org.scalajs.dom
import org.scalajs.dom.{HTMLAnchorElement, HTMLDivElement}
import scala.concurrent.duration.FiniteDuration

case class LocationTimeDirection(
  locationWithTime: LocationWithTime,
  routeSegment: RouteSegment)

object Components {
  def FullApp(
    javaClock: Clock,
  ) = {

    // Clean up SBG icon references.
    val appMode =
      if (dom.document.URL.contains("localhost")) AppMode.Local
      else AppMode.Production

    println(s"appMode: $appMode")
    val db: Persistence = Persistence()

    val clockTicks = new EventBus[Unit]

    def currentWallTime(
      javaClock: Clock,
    ) =
      WallTime(
        OffsetDateTime
          .now(javaClock)
          .toLocalTime
          .format(
            DateTimeFormatter.ofPattern("HH:mm"),
          ),
      )

    val initialTime =
      currentWallTime:
        javaClock

    val timeStamps: Signal[WallTime] = clockTicks.events
      .scanLeft(
        initialTime,
      )(
        (
          _,
          _,
        ) => currentWallTime(javaClock),
      )

    val $plan: Var[Plan] = Var(
      db.retrieveDailyPlanOnly.getOrElse(Plan(Seq.empty)),
    )

    val addingNewRoute: Var[Boolean] = Var(
      $plan.now().routeSegments.isEmpty, // If no segments , assume we want to add more
    )

    val selectedStop: Var[Option[(BusScheduleAtStop, RouteSegment)]] =
      Var(None)

    val whatToShowBetter
      : Signal[ReactiveHtmlElement[HTMLDivElement]] =
      selectedStop.signal
        .map {
          case Some((busScheduleAtStop, routeSegment)) =>
            val res =
              UpcomingStops(
                busScheduleAtStop,
                routeSegment,
                $plan.writer
                  .contramap[LocationTimeDirection] { ltd =>
                    selectedStop.set(None)
                    val res =
                      TimeCalculations
                        .updateSegmentFromArbitrarySelection(
                          ltd,
                          $plan.now(),
                        ) match {
                        case Left(failure) =>
                          throw new Exception(failure)
                        case Right(segment) => segment
                      }
                    db.saveDailyPlanOnly(res)
                    addingNewRoute.set {
                      false
                    }
                    res
                  },
              )

            import scala.scalajs.js.timers._
            setTimeout(200)(
              dom.document
                .getElementById("selected-time")
                .scrollIntoView(
                  top = false,
                ),
            )
            res
          case None =>
            Components.PlanElement(
              timeStamps,
              db,
              $plan,
              initialTime,
              addingNewRoute,
              selectedStop.writer,
            )
        }

    div(
      onMountCallback: context =>
        db.initializeOrResetStorage(),
      RepeatingElement()
        .repeatWithInterval( // This acts like a Dune thumper
          (),
          new FiniteDuration(200,
                             scala.concurrent.duration.SECONDS,
          ), // TODO Make low again
        ) --> clockTicks,
      div(
        div(
          cls := ElementNames.BoxClass,
          idAttr := "container",
          child <-- whatToShowBetter, // **THIS IS THE IMPORTANT STUFF** The fact that it's hard to see means I need to remove other bullshit
          timeStamps --> Observer[WallTime](
            onNext = localTime =>
              desiredAlarms
                .dequeueAll(busTime =>
                  localTime
                    .between(busTime)
                    .toMinutes <= NotificationStuff.headsUpAmount.toMinutes,
                )
                .foreach(
                  Experimental.Notifications.createJankyBusAlertInSideEffectyWay,
                ),
          ),
          Option.when(appMode == AppMode.Local)(
            Experimental.Sandbox(
              timeStamps,
            ),
          ),
        ),
      ),
    )
  }

  def PlanElement(
    timeStamps: Signal[WallTime],
    db: Persistence,
    $plan: Var[Plan],
    initialTime: WallTime,
    addingNewRoute: Var[Boolean],
    scheduleSelector: Observer[
      Option[(BusScheduleAtStop, RouteSegment)],
    ],
  ) =
    div(
      copyButtons($plan.signal),
      children <-- $plan.signal
        .map(_.routePieces.zipWithIndex)
        // TODO. Nail down key that works for all situations.
        //    Currently, If I use the index, my swipe transitions are good, but my deletions are bad.
        //    If I use the id via `_._1.id`, my deletions are good, but my swipe transitions are bad.
        //    What I really need is an id associated with the RouteSegment that is unique added to a plan, but will be maintained when I do a swipe that updates the start/end times of that element in the plan.
        //    This could require changing the visibility of the RouteSegment constructor, and/or making the id a constructor arg.
        .splitTransition(_._1.id) {
          case (_, (routePiece, idx), routePieceSignal, transition) =>
            val legDeleter =
              Observer { (rs: RouteSegment) =>
                val plan = $plan.now()
                println("More narrow deletion")
                val newPlan =
                  plan
                    .copy(l = plan.l.filterNot(_ == rs))
                db.saveDailyPlanOnly(newPlan)
                $plan.set(newPlan)
                if (newPlan.l.isEmpty) {
                  addingNewRoute.set {
                    true
                  }
                }
              }
            val segmentUpdater: Observer[RouteSegment] =
              $plan.writer
                .contramap[RouteSegment] { segment =>
                  val plan = $plan.now()
                  val updatedPlan =
                    Plan(
                      plan.l.map {
                        case rs if rs.id == segment.id =>
                          segment
                        case rs =>
                          rs
                      },
                    )
                  db.saveDailyPlanOnly(updatedPlan)
                  updatedPlan
                }
            div(
              child <-- routePieceSignal
                .map {
                  case (routePieceInner, idx) =>
                    div(
                      transition.height,
                      routePieceInner match {
                        case r: RouteGap =>
                          div(
                            textAlign := "center",
                            paddingTop := "1.5em",
                            paddingBottom := "1.5em",
                            span(
                              cls := "time-at-stop",
                              r.endTime
                                .between(r.start)
                                .humanFriendly,
                            ),
                          )
                        case rs: RouteSegment =>
                          RouteLegElement(
                            rs,
                            addingNewRoute,
                            scheduleSelector,
                            transition,
                            legDeleter,
                            segmentUpdater,
                          )

                      },
                    )
                },
            )
        },
      div(
        cls := "add-new-route-section",
        child <-- addingNewRoute.signal.map {
          case false =>
            div(
              cls := "centered",
              button(
                cls := "button",
                "Add new route!",
                onClick --> Observer { _ =>
                  addingNewRoute.set {
                    true
                  }
                },
              ),
            )
          case true =>
            div(
              StopSelector(
                CompleteStopList.values,
                $plan,
                db,
                timeStamps,
                addingNewRoute,
              ),
            )
        },
      ),
    )

  def deleteButton(
    routeSegment: RouteSegment,
    addingNewRoute: Var[Boolean],
    legDeleter: Observer[RouteSegment],
  ): ReactiveHtmlElement[HTMLAnchorElement] =
    a(
      cls := "link transit-period-delete",
      onClick.mapTo(routeSegment) --> legDeleter,
      SvgIcon.squareMinus("delete"),
    )

  def transitSegment(
    routeSegment: RouteSegment,
    addingNewRoute: Var[Boolean],
    legDeleter: Observer[RouteSegment],
  ) =
    div(
      cls := "transit-period",
      div(
        cls := "transit-period-icon ",
        SvgIcon.arrowDown("plain-white plan-segment-divider"),
      ),
      span(
        cls := "transit-period-duration transit-time",
        routeSegment.start.t
          .between(routeSegment.end.t)
          .humanFriendly,
      ),
      deleteButton(routeSegment, addingNewRoute, legDeleter),
    )

  def RouteLegElement(
    routeSegment: RouteSegment,
    addingNewRoute: Var[Boolean],
    scheduleSelector: Observer[
      Option[(BusScheduleAtStop, RouteSegment)],
    ],
    transition: Transition,
    legDeleter: Observer[RouteSegment],
    segmentUpdater: Observer[RouteSegment],
  ) = {

    val res =
      div(
        transition.width,
        TouchControls.swipeProp {
          case Swipe.Left =>
            routeSegment.routeWithTimes
              .nextAfter(routeSegment)
              .foreach(nextSegment =>
                segmentUpdater.onNext(
                  nextSegment,
                ),
              )
          case Swipe.Right =>
            routeSegment.routeWithTimes
              .nextBefore(routeSegment)
              .foreach(nextSegment =>
                segmentUpdater.onNext(
                  nextSegment,
                ),
              )
        },
        cls := "plan-segments box",
        stopInfo(routeSegment,
                 routeSegment.start,
                 routeSegment.routeWithTimes,
                 scheduleSelector,
                 transition,
        ),
        transitSegment(
          routeSegment,
          addingNewRoute,
          legDeleter,
        ),
        stopInfo(routeSegment,
                 routeSegment.end,
                 routeSegment.routeWithTimes,
                 scheduleSelector,
                 transition,
        ),
      )

    res.ref.addEventListener(
      "long-press",
      _ => println("Long pressed!"),
    )

    res
  }

  def stopInfo(
    routeSegment: RouteSegment,
    stop: LocationWithTime,
    routeWithTimes: RouteWithTimes,
    scheduleSelector: Observer[
      Option[(BusScheduleAtStop, RouteSegment)],
    ],
    transition: Transition,
  ) =
    div(
      cls := "stop-information",
      div(cls := "stop-name", div(stop.l.name)),
      div(cls := "stop-alt-name", div(stop.l.altName)),
      div(
        div(
          routeWithTimes.allStops
            .filter(_.location == stop.l)
            .map { scheduleAtStop =>
              StopTimeInfoForLocation(stop.t,
                                      scheduleAtStop,
                                      scheduleSelector,
                                      routeSegment,
              )
            },
        ),
      ),
    )

  def copyButtons(
    $plan: Signal[Plan],
  ) =
    div(
      child <-- $plan.map { plan =>
        if (plan.routeSegments.isEmpty)
          div()
        else
          div(
            button(
              cls := "button m-2",
              "Copy Text",
              onClick --> Observer { _ =>
                dom.window.navigator.clipboard
                  .writeText(plan.plainTextRepresentation)
              },
            ),
            button(
              cls := "button m-2",
              "Copy App Link",
              onClick --> Observer { _ =>
                // TODO Base this on page mode Parameter and don't hard code URLs at this level
                val url =
                  if (dom.document.URL.contains("localhost"))
                    s"http://localhost:8000/index_dev.html?plan=${UrlEncoding.encode(plan)}"
                  else
                    s"https://rtabus.netlify.app/?plan=${UrlEncoding.encode(plan)}"

                dom.window.navigator.clipboard
                  .writeText(url)
              },
            ),
          )
      },
    )

  def rightLegOnRightRoute(
    start: Location,
    end: Location,
    plan: Plan,
    pageLoadTime: WallTime,
  ): Option[RouteSegment] = {

    val routeSegmentsO =
      RTA.Southbound.fullSchedule
        .segment(start, end)
        .orElse(RTA.Northbound.fullSchedule.segment(start, end))
//        .getOrElse(
//          throw new IllegalStateException(
//            "No route leg available in either route B",
//          ),
//        )

    routeSegmentsO
      .flatMap(routeSegments =>
        routeSegments
          .find { l =>
            val lastArrivalTime =
              plan.l.lastOption
                .map(_.end.t)
            val cutoff =
              lastArrivalTime.getOrElse(pageLoadTime)
            l.start.t.isAfter(cutoff)
          }
          .orElse {
            routeSegments.headOption
          },
      )

  }

  def StopSelector(
    locations: Seq[Location],
    $plan: Var[Plan],
    db: Persistence,
    $now: Signal[WallTime],
    addingNewRoute: Var[Boolean], // TODO Smaller type
  ) =
    val startingPoint: Var[Option[Location]] = Var(None)
    val $locationsVar: Var[Seq[(Location, Int)]] = Var(Seq.empty)
    val $locations: Signal[Seq[(Location, Int)]] =
      $locationsVar.signal

    div(
      onMountCallback { ctx =>
        locations.zipWithIndex.foreach(l =>
          import scala.scalajs.js.timers._
          setTimeout(l._2 * 30)(
            $locationsVar.update(_ :+ l),
          ),
        )
      },
      div(
        children <-- $locations.splitTransition(identity) {
          case (_, (location, _), _, transition) =>
            div(
              transition.width,
              transition.height,
              child <-- $now.map {
                now =>
                  button(
                    disabled <-- startingPoint.signal.map {
                      case Some(startingPointNow)
                          if startingPointNow == location =>
                        false
                      case Some(other) =>
                        rightLegOnRightRoute(
                          other,
                          location,
                          $plan.now(),
                          now,
                        ).isEmpty
                      case None => false
                    },
                    cls := "button m-2",
                    onClick --> Observer {
                      _ =>
                        startingPoint.update {
                          case Some(startingPointNow)
                              if startingPointNow == location =>
                            None
                          case Some(other) =>
                            val matchingLegO =
                              rightLegOnRightRoute(
                                other,
                                location,
                                $plan.now(),
                                now,
                              )

                            matchingLegO match
                              case Some(matchingLeg) =>
                                $plan.update { case oldPlan =>
                                  val newPlan =
                                    oldPlan.copy(l =
                                      oldPlan.l :+ matchingLeg,
                                    )
                                  db.saveDailyPlanOnly(newPlan)
                                  addingNewRoute.set(false)
                                  newPlan
                                }
                                Some(other)
                              case None =>
                                println(
                                  "giving up and deselecting starting point",
                                )
                                None

                          case None =>
                            Some(location)
                        }
                    },
                    cls <-- startingPoint.signal.map {
                      case Some(startingPointNow)
                          if startingPointNow == location =>
                        "is-primary"
                      case Some(_) => "is-info"
                      case None    => "is-info"
                    },
                    location.name,
                  )
              },
            )
        },
      ),
    )

  def StopTimeInfoForLocation(
    stopTime: WallTime,
    busScheduleAtStop: BusScheduleAtStop,
    scheduleSelector: Observer[
      Option[(BusScheduleAtStop, RouteSegment)],
    ],
    routeSegment: RouteSegment,
  ): ReactiveHtmlElement[HTMLDivElement] =
    div(
      button(
        cls := "arrival-time button open-arrival-time-modal",
        onClick.preventDefault.map { _ =>
          Some((busScheduleAtStop, routeSegment))
        } --> scheduleSelector,
        stopTime.toDumbAmericanString,
      ),
    )
}
