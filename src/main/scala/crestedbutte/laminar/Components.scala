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
import crestedbutte.routes.{
  CompleteStopList,
  RouteWithTimes,
  RtaNorthbound,
  RtaSouthbound,
}
import java.time.format.DateTimeFormatter
import java.time.{Clock, OffsetDateTime}
import org.scalajs.dom
import org.scalajs.dom.{HTMLAnchorElement, HTMLDivElement}
import scala.concurrent.duration.FiniteDuration

case class LocationTimeDirection(
  locationWithTime: LocationWithTime,
  routeSegment: RouteSegment)

object Components {

  def PlanElement(
    db: Persistence,
    $plan: Var[Plan],
    initialTime: WallTime,
    timestamp: WallTime,
    addingNewRoute: Var[Boolean],
    scheduleSelector: Observer[
      Option[(BusScheduleAtStop, RouteSegment)],
    ],
  ) =
    div(
      child <-- $plan.signal.map { plan =>
        val segments = plan.routeSegments
        val routePieces: Seq[RoutePiece] =
          if (segments.isEmpty)
            segments
          else
            segments.tail
              .foldLeft[Seq[RoutePiece]](Seq(segments.head)) {
                case (acc, next) =>
                  acc.last match
                    case RouteSegment(r, s, e) =>
                      (acc :+ RouteGap(e.t, next.start.t)) :+ next
                    case _ =>
                      ??? // Eh, annoying, but not nearly as bad as the previous muck
              }

        val $planSegments: Var[Seq[(RoutePiece, Int)]] =
          Var(Seq.empty)

        import scala.scalajs.js.timers._
        routePieces.zipWithIndex.foreach(l =>
          setTimeout(l._2 * 150)(
            $planSegments.update(_ :+ l),
          ),
        )

        val segmentContentNifty
          : Signal[Seq[ReactiveHtmlElement[HTMLDivElement]]] =
          $planSegments.signal
            .splitTransition(identity) {
              case (_, (routePiece, i), _, transition) =>
                routePiece match {
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
                      i / 2, // hack to unfuck indices now that the gaps get them too
                      db,
                      $plan,
                      addingNewRoute,
                      timestamp,
                      scheduleSelector,
                    ).amend(
                      transition.height,
                    )

                }

            }

        div(
          if (segments.isEmpty)
            div()
          else
            copyButtons(plan),
          children <-- segmentContentNifty,
          div(
            cls := "add-new-route-section",
            child <-- addingNewRoute.signal.map {
              case false =>
                div(
                  cls := "centered",
                  button(
                    cls := "button",
                    "Add new route",
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
                    initialTime,
                    addingNewRoute,
                  ),
                )
            },
          ),
        )
      },
    )

  def deleteButton(
    routeSegment: RouteSegment,
    $plan: Var[Plan],
    db: Persistence,
    addingNewRoute: Var[Boolean],
  ): ReactiveHtmlElement[HTMLAnchorElement] =
    a(
      cls := "link transit-period-delete",
      // TODO Should this Observer/behavior be defined above?
      onClick --> Observer { _ =>
        val plan = $plan.now()
        val newPlan =
          plan.copy(l = plan.l.filterNot(_ == routeSegment))
        db.saveDailyPlanOnly(newPlan)
        $plan.set(newPlan)
        if (newPlan.l.isEmpty) {
          addingNewRoute.set {
            true
          }
        }
      },
      SvgIcon("glyphicons-basic-842-square-minus.svg",
              clsName = "delete",
      ),
    )

  def transitSegment(
    routeSegment: RouteSegment,
    $plan: Var[Plan],
    db: Persistence,
    addingNewRoute: Var[Boolean],
  ) =
    div(
      cls := "transit-period",
      div(
        cls := "transit-period-icon ",
        SvgIcon("glyphicons-basic-211-arrow-down.svg",
                "plain-white plan-segment-divider",
        ),
      ),
      span(
        cls := "transit-period-duration transit-time",
        routeSegment.start.t
          .between(routeSegment.end.t)
          .humanFriendly,
      ),
      deleteButton(routeSegment, $plan, db, addingNewRoute),
    )

  def RouteLegElement(
    routeSegment: RouteSegment,
    planIndex: Int,
    db: Persistence,
    $plan: Var[Plan],
    addingNewRoute: Var[Boolean],
    timestamp: WallTime,
    scheduleSelector: Observer[
      Option[(BusScheduleAtStop, RouteSegment)],
    ],
  ) = {

    val planSwipeUpdater: Observer[(Int, Option[RouteSegment])] =
      $plan.writer.contramap[(Int, Option[RouteSegment])] {
        (
          idx,
          segmentO,
        ) =>
          segmentO match
            case Some(segment) =>
              val plan = $plan.now()
              val updatedPlan =
                plan.copy(l = plan.l.updated(idx, segment))
              db.saveDailyPlanOnly(updatedPlan)
              updatedPlan
            case None => $plan.now()
      }

    val res =
        div(
          TouchControls.swipeProp {
            case Swipe.Left =>
              planSwipeUpdater.onNext(
                planIndex,
                routeSegment.routeWithTimes.nextAfter(routeSegment),
              )
            case Swipe.Right =>
              planSwipeUpdater.onNext(
                planIndex,
                routeSegment.routeWithTimes.nextBefore(routeSegment),
              )
          },
          cls := "plan-segments box",
          if (timestamp.isAfter(routeSegment.start.t))
            opacity := 0.5
          else
            cls := "",
          stopInfo(routeSegment,
                   routeSegment.start,
                   routeSegment.routeWithTimes,
                   timestamp,
                   scheduleSelector,
          ),
          transitSegment(
            routeSegment,
            $plan,
            db,
            addingNewRoute,
          ),
          stopInfo(routeSegment,
                   routeSegment.end,
                   routeSegment.routeWithTimes,
                   timestamp,
                   scheduleSelector,
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
    timestamp: WallTime, // TODO *should* we be using this?
    scheduleSelector: Observer[
      Option[(BusScheduleAtStop, RouteSegment)],
    ],
  ) =
    div(
      width := "100%",
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

  def FullApp(
    pageMode: AppMode,
    javaClock: Clock,
  ) = {
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

    val upcomingArrivalData =
      timeStamps
        .map { timestamp =>
          Components.PlanElement(
            db,
            $plan,
            initialTime,
            timestamp,
            addingNewRoute,
            selectedStop.writer,
          )
        }

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
                        )
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
                  //              { behavior: "instant", block: "end" }
                ),
            )
            res
          case None =>
            div(
              child <-- upcomingArrivalData,
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
          Option.when(pageMode == AppMode.dev)(
            Experimental.Sandbox(
              timeStamps,
            ),
          ),
        ),
      ),
    )
  }

  def copyButtons(
    plan: Plan,
  ) =
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
          val url =
            if (dom.document.URL.contains("localhost"))
              s"http://localhost:8000/index_dev.html?plan=${UrlEncoding.encode(plan)}"
            else
              s"https://cbbus.netlify.app/?plan=${UrlEncoding.encode(plan)}"

          dom.window.navigator.clipboard
            .writeText(url)
        },
      ),
    )

  def rightLegOnRightRoute(
    start: Location,
    end: Location,
    plan: Plan,
    pageLoadTime: WallTime,
  ): Option[RouteSegment] = {

    val routeSegmentsO =
      RtaSouthbound.fullSchedule
        .segment(start, end)
        .orElse(RtaNorthbound.fullSchedule.segment(start, end))
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
    initialTime: WallTime,
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
                      initialTime,
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
                            initialTime,
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
              ),
            )
        },
      ),
    )

  def SafeRideLink(
    safeRideRecommendation: LateNightRecommendation,
  ) =
    div(
      cls := "late-night-call-button",
      a(
        href := s"tel:${safeRideRecommendation.phoneNumber}",
        cls := "link",
        button(
          cls := "button",
          SvgIcon("glyphicons-basic-465-call.svg").amend(
            alt := "Call Late Night Shuttle!",
          ),
          safeRideRecommendation.message,
        ),
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
