package crestedbutte.laminar

import animus.*
import com.billding.time.WallTime
import com.raquo.laminar.api.L.*
import com.raquo.laminar.nodes.ReactiveHtmlElement
import crestedbutte.*
import crestedbutte.NotificationStuff.desiredAlarms
import crestedbutte.dom.BulmaLocal
import crestedbutte.dom.BulmaLocal.UpcomingStops
import crestedbutte.dom.{BulmaLocal, StopContext}
import crestedbutte.laminar.TouchControls.Swipe
import crestedbutte.pwa.Persistence
import crestedbutte.routes.{CompleteStopList, RTA, RouteWithTimes}
import java.time.format.DateTimeFormatter
import java.time.{Clock, OffsetDateTime}
import org.scalajs.dom
import org.scalajs.dom.{HTMLAnchorElement, HTMLDivElement}
import scala.scalajs.js
import scala.scalajs.js.timers._
import scala.concurrent.duration.FiniteDuration

case class LocationTimeDirection(
  locationWithTime: LocationWithTime,
  routeSegment: RouteSegment)

case class SelectedStopInfo(
  busScheduleAtStop: BusScheduleAtStop,
  routeSegment: RouteSegment,
  context: StopContext)

object Components {
  def FullApp(
    javaClock: Clock,
  ) = {

    val appMode =
      if (dom.document.URL.contains("localhost")) AppMode.Local
      else AppMode.Production

    println(s"appMode: $appMode")
    val db: Persistence = Persistence()

    val frontEndClock = new FrontEndClock(javaClock)
    val timeStamps = frontEndClock.timeStamps

    val $plan: Var[Plan] = Var(
      db.getCurrentPlan.getOrElse(Plan(Seq.empty)),
    )

    val addingNewRoute: Var[Boolean] = Var(
      $plan.now().routeSegments.isEmpty, // If no segments , assume we want to add more
    )

    val selectedStop: Var[Option[SelectedStopInfo]] =
      Var(None)

    val whatToShowBetter
      : Signal[ReactiveHtmlElement[HTMLDivElement]] =
      selectedStop.signal
        .map {
          case Some(selectedStopInfo) =>
            val res =
              UpcomingStops(
                selectedStopInfo.busScheduleAtStop,
                selectedStopInfo.routeSegment,
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
                selectedStopInfo.context,
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
              addingNewRoute,
              selectedStop.writer,
            )
        }

    div(
      onMountCallback: context =>
        db.initializeOrResetStorage(),
      frontEndClock.clockElement,
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
          Option.when(appMode == AppMode.Local && false)(
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
    addingNewRoute: Var[Boolean],
    scheduleSelector: Observer[
      Option[SelectedStopInfo],
    ],
  ) =
    div(
      copyButtons($plan.signal),
      children <-- $plan.signal
        .map(_.routePieces.zipWithIndex)
        .splitTransition(_._1.id) {
          case (_, (routePiece, idx), routePieceSignal, transition) =>
            val legDeleter =
              Observer { (rs: RouteSegment) =>
                val plan = $plan.now()
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
              child <--
                routePieceSignal
                  .map {
                    case (routePieceInner, idx) =>
                      div(
                        transition.height,
                        routePieceInner match {
                          case r: RouteGap =>
                            div(
                              cls := "route-gap",
                              div(
                                cls := "route-gap-indicator",
                              ),
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
      Option[SelectedStopInfo],
    ],
    transition: Transition,
    legDeleter: Observer[RouteSegment],
    segmentUpdater: Observer[RouteSegment],
  ) = {

    val localSelection: Var[RouteSegment] = Var(routeSegment)

    def segmentEditorCarousel() = {
      val prev = routeSegment.routeWithTimes.nextBefore(routeSegment)
      val current = routeSegment
      val next = routeSegment.routeWithTimes.nextAfter(routeSegment)

      def neighbors(
        seg: RouteSegment,
      ): (Option[RouteSegment], RouteSegment, Option[RouteSegment]) =
        (
          seg.routeWithTimes.nextBefore(seg),
          seg,
          seg.routeWithTimes.nextAfter(seg),
        )

      div(
        cls := "segment-editor",
        div(
          cls := "segment-editor-header",
          s"${current.start.l.name} → ${current.end.l.name}",
        ),
        div(
          cls := "segment-editor-carousel",
          // Previous (left)
          prev match {
            case Some(prev) =>
              div(
                cls := "carousel-card prev clickable",
                onClick --> Observer { _ =>
                  segmentUpdater.onNext(prev)
                },
                div(prev.start.t.toDumbAmericanStringWithoutDayTime),
                div("to"),
                div(prev.end.t.toDumbAmericanStringWithoutDayTime),
              )
            case None =>
              div(
                cls := "carousel-card prev empty",
              )
          },
          // Current (center) with slide animation
          div(
            cls := "carousel-card current",
            div(
              div(
                current.start.t.toDumbAmericanString,
              ),
              div("to"),
              div(
                current.end.t.toDumbAmericanString,
              ),
            ),
          ),
          // Next (right)
          next match {
            case Some(next) =>
              div(
                cls := "carousel-card next clickable",
                onClick --> Observer { _ =>
                  localSelection.set(next)
                  segmentUpdater.onNext(next)
                },
                div(next.start.t.toDumbAmericanStringWithoutDayTime),
                div("to"),
                div(next.end.t.toDumbAmericanStringWithoutDayTime),
              )
            case None =>
              div(
                cls := "carousel-card next empty",
              )
          },
        ),
      )
    }

    div(
      transition.height,
      cls := "plan-segments box",
      segmentEditorCarousel(),
      // Show transit time and delete segment button alongside the editor
      transitSegment(
        routeSegment,
        addingNewRoute,
        legDeleter,
      ),
    )

  }

  def stopInfo(
    routeSegment: RouteSegment,
    stop: LocationWithTime,
    routeWithTimes: RouteWithTimes,
    scheduleSelector: Observer[
      Option[SelectedStopInfo],
    ],
    context: StopContext,
  ) =
    div(
      cls := "stop-information",
      div(
        cls := "stop-name",
        div(stop.l.name),
      ),
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
                                      context,
              )
            },
        ),
      ),
    )

  def animatedButton(
    text: String,
    additionalClasses: String = "",
    onClickAction: () => Unit,
  ) =
    button(
      cls := s"button $additionalClasses",
      text,
      onClick --> Observer { _ =>
        onClickAction()
      },
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
            animatedButton(
              "Copy Text",
              "m-2",
              () => {
                val text = plan.plainTextRepresentation

                // Try to use Web Share API if available (works on mobile devices)
                if (
                  js.typeOf(
                    dom.window.navigator
                      .asInstanceOf[js.Dynamic]
                      .share,
                  ) != "undefined"
                ) {
                  dom.window.navigator
                    .asInstanceOf[js.Dynamic]
                    .share(
                      js.Dynamic.literal(
                        title = "Bus Schedule",
                        text = text,
                      ),
                    )
                }
                else {
                  // Fallback to clipboard copy for desktop browsers
                  dom.window.navigator.clipboard.writeText(text)
                }
              },
            ),
            animatedButton(
              "Copy App Link",
              "m-2",
              () => {
                // TODO Base this on page mode Parameter and don't hard code URLs at this level
                val url =
                  if (dom.document.URL.contains("localhost"))
                    s"http://localhost:8000/index.html?plan=${UrlEncoding.encode(plan)}"
                  else
                    s"https://rtabus.netlify.app/?plan=${UrlEncoding.encode(plan)}"

                // Try to use Web Share API if available (works on mobile devices)
                if (
                  js.typeOf(
                    dom.window.navigator
                      .asInstanceOf[js.Dynamic]
                      .share,
                  ) != "undefined"
                ) {
                  dom.window.navigator
                    .asInstanceOf[js.Dynamic]
                    .share(
                      js.Dynamic.literal(
                        title = "Bus Schedule Link",
                        url = url,
                      ),
                    )
                }
                else {
                  // Fallback to clipboard copy for desktop browsers
                  dom.window.navigator.clipboard.writeText(url)
                }
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
    addingNewRoute: Var[
      Boolean,
    ], // TODO Make this an Observer[Boolean]
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
      Option[SelectedStopInfo],
    ],
    routeSegment: RouteSegment,
    context: StopContext,
  ): ReactiveHtmlElement[HTMLDivElement] =
    div(
      span(
        cls := "arrival-time label",
        stopTime.toDumbAmericanString,
      ),
    )
}
