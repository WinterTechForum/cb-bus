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

    // Demo wheel with sample data
    val demoItems = Seq(
      s"Apple\nto\nAppleDestination",
      "Banana",
      "Cherry",
      "Date",
      "Elderberry",
      "Fig",
      "Grape",
      "Honeydew",
      "Kiwi",
      "Lemon",
      "Mango",
      "Orange",
    )
    val (wheelElement, selectedValue) =
      ScrollingWheel.ScrollingWheel(demoItems, item => div(item), 0)

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

          // Scrolling wheel demo at the top
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
    // Track a recently deleted segment and its original index for undo
    val pendingUndo: Var[Option[(RouteSegment, Int)]] = Var(None)
    var pendingUndoTimer: Option[SetTimeoutHandle] = None

    def startUndoTimer(): Unit =
      // Reset any previous timer
      pendingUndoTimer.foreach(clearTimeout)
      pendingUndoTimer = Some(
        setTimeout(4000) {
          pendingUndo.set(None)
          pendingUndoTimer = None
        },
      )

    div(
      copyButtons($plan.signal),
      children <-- $plan.signal
        .map(_.routePieces)
        .splitTransition(_.id) {
          case (_, routePiece, routePieceSignal, transition) =>
            div(
              child <--
                routePieceSignal
                  .map {
                    routePieceInner =>
                      div(
                        transition.height,
                        routePieceInner match {
                          case r: RouteGap =>
                            println(s"RouteGap: ${r.id}")
                            div(
                              cls := "route-gap",
                              div(
                                cls := "route-gap-indicator",
                              ),
                              span(
                                transition.width,
                                cls := "time-at-stop",
                                if (r.endTime.isBefore(r.start))
                                  "Next Day"
                                else
                                  r.endTime
                                    .between(r.start)
                                    .humanFriendly,
                              ),
                            )
                          case rs: RouteSegment =>
                            RouteLegElement(
                              rs, // TODO I **must* figure out how to rework this so that RouteLegElement takes a signal, and is not rebuilt every time the segment is updated.
                              addingNewRoute,
                              scheduleSelector,
                              legDeleter =
                                Observer {
                                  (rs: RouteSegment) =>
                                    val plan = $plan.now()
                                    val originalIndex =
                                      plan.l.indexOf(rs)
                                    val newPlan =
                                      plan.copy(l =
                                        plan.l.filterNot(_ == rs),
                                      )

                                    // Apply deletion immediately
                                    db.saveDailyPlanOnly(newPlan)
                                    $plan.set(newPlan)
                                    if newPlan.l.isEmpty then
                                      addingNewRoute.set(true)

                                    // Enable undo for a few seconds
                                    if originalIndex >= 0 then
                                      pendingUndo.set(
                                        Some((rs, originalIndex)),
                                      )
                                      startUndoTimer()
                                },
                              segmentUpdater = $plan.writer
                                .contramap[RouteSegment] { segment =>
                                  val plan = $plan.now()
                                  val updatedPlan =
                                    Plan(
                                      plan.l.map {
                                        case rs
                                            if rs.id == segment.id =>
                                          segment
                                        case rs =>
                                          rs
                                      },
                                    )
                                  db.saveDailyPlanOnly(updatedPlan)
                                  updatedPlan
                                },
                              // Append a new segment to the end of the plan
                              segmentAppender = $plan.writer
                                .contramap[RouteSegment] {
                                  newSegment =>
                                    val plan = $plan.now()
                                    val updatedPlan = plan
                                      .copy(l = plan.l :+ newSegment)
                                    db.saveDailyPlanOnly(updatedPlan)
                                    addingNewRoute.set(false)
                                    updatedPlan
                                },
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
              styleAttr := "display: flex; gap: 0.5rem; flex-wrap: wrap; justify-content: center;",
              button(
                cls := "button is-info",
                "New route",
                onClick --> Observer { _ =>
                  addingNewRoute.set {
                    true
                  }
                },
              ),
              button(
                cls := "button is-info",
                "Return trip",
                onClick --> Observer { _ =>
                  val plan = $plan.now()
                  val maybeLastSeg = plan.l.lastOption
                  maybeLastSeg.foreach { lastSeg =>
                    val maybeReturn =
                      rightLegOnRightRoute(
                        lastSeg.end.l,
                        lastSeg.start.l,
                        Plan(Seq.empty),
                        lastSeg.end.t,
                      )
                    maybeReturn.foreach { newSeg =>
                      val updatedPlan =
                        plan.copy(l = plan.l :+ newSeg)
                      db.saveDailyPlanOnly(updatedPlan)
                      $plan.set(updatedPlan)
                      addingNewRoute.set(false)
                    }
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
      // Small undo UI that appears for a few seconds after deletion
      child <-- pendingUndo.signal.map {
        case Some((seg, idx)) =>
          div(
            cls := "undo-banner",
            styleAttr := "display: flex; align-items: center; justify-content: center; gap: 0.5rem; margin-top: 0.5rem;",
            span("Segment deleted."),
            button(
              cls := "button is-small is-link is-light",
              "Undo",
              onClick --> Observer { _ =>
                pendingUndo.now().foreach { case (s, i) =>
                  val planNow = $plan.now()
                  val restoredPlan =
                    planNow.copy(l =
                      (planNow.l.take(i) :+ s) ++ planNow.l.drop(i),
                    )
                  db.saveDailyPlanOnly(restoredPlan)
                  $plan.set(restoredPlan)
                  addingNewRoute.set(false)
                }
                pendingUndoTimer.foreach(clearTimeout)
                pendingUndoTimer = None
                pendingUndo.set(None)
              },
            ),
          )
        case None =>
          div()
      },
    )

  def deleteButton(
    routeSegment: RouteSegment,
    addingNewRoute: Var[Boolean],
    legDeleter: Observer[RouteSegment],
  ): ReactiveHtmlElement[HTMLAnchorElement] =
    a(
      cls := "link transit-period-delete",
      onClick.mapTo(routeSegment) --> legDeleter,
      SvgIcon.squareMinus("delete filter-white"),
    )

  def transitSegment(
    routeSegment: RouteSegment,
    addingNewRoute: Var[Boolean],
    legDeleter: Observer[RouteSegment],
  ) =
    div(
      cls := "transit-period",
      styleAttr := "display: flex; flex-direction: column; align-items: center; justify-content: space-between; height: 100%;",
    )

  def RouteLegElement(
    routeSegment: RouteSegment,
    addingNewRoute: Var[Boolean],
    scheduleSelector: Observer[
      Option[SelectedStopInfo],
    ],
    legDeleter: Observer[RouteSegment],
    segmentUpdater: Observer[RouteSegment],
    segmentAppender: Observer[RouteSegment],
  ) = {
    println("Making a new RouteLegElement for : " + routeSegment.id)

    val allSegments =
      routeSegment.routeWithTimes
        .allRouteSegmentsWithSameStartAndStop(routeSegment)

    val returnSymbols = List(
      "↩", // Return arrow
      "⮐", // Return symbol
      "⟲", // Clockwise gapped circle arrow
      "🔄", // Clockwise arrows
      "⇄", // Left right arrow
      "⇋", // Left right wave arrow
      "↺", // Anticlockwise open circle arrow
      "⮌", // Clockwise top semicircle arrow
    )

    val offsetPx: Var[Double] = Var(0.0)

    val (swipeModifier, allowVerticalDrag) =
      TouchControls.swipeToDelete(
        deleteTriggerRatio = 0.35,
        minTriggerPx = 100.0,
        offsetPx = offsetPx,
        onDelete = () => legDeleter.onNext(routeSegment),
      )

    val (wheelElement, selectedValue) =
      ScrollingWheel.ScrollingWheel(
        allSegments,
        item =>
          div(
            item.s.t.toDumbAmericanString + "→" + item.e.t.toDumbAmericanString,
          ),
        0,
        Some(routeSegment),
        allowVerticalDrag,
      )

    div(
      cls := "plan-segments box",
      styleAttr := "position: relative; overflow: hidden;",
      // Slidable content
      div(
        styleAttr := "display: flex; align-items: flex-start; width: 100%;",
        styleProp("position") := "relative",
        styleProp("z-index") := "1",
        styleProp("transition") := "transform 180ms ease",
        styleProp("transform") <-- offsetPx.signal.map(px =>
          s"translateX(-${px}px)",
        ),
        // Touch handlers for swipe-to-reveal (encapsulated)
        swipeModifier,
        div(
          styleAttr := "flex: 3; display: flex; flex-direction: column;",
          div(
            cls := "segment-editor-header",
            s"${routeSegment.start.l.name} → ${routeSegment.end.l.name}",
          ),
          selectedValue --> segmentUpdater, // TODO Eventually this should be restored
          wheelElement,
        ),
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
      cls := s"button is-info $additionalClasses",
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
              "Copy Link",
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
