package crestedbutte.laminar

import animus.*
import com.billding.time.WallTime
import com.raquo.laminar.api.L.*
import com.raquo.laminar.nodes.ReactiveHtmlElement
import crestedbutte.*
import crestedbutte.NotificationStuff.desiredAlarms
import crestedbutte.dom.StopContext
import crestedbutte.laminar.TouchControls.Swipe
import crestedbutte.pwa.Persistence
import crestedbutte.{RTA, RouteWithTimes}
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
  def NotificationToggleButton(
    $plan: Var[Plan],
    timeStamps: Signal[WallTime],
    buttonWidth: Int,
  ) = {
    val notificationsEnabled: Var[Boolean] = Var(false)

    button(
      cls := "button button-fixed-width",
      child <-- notificationsEnabled.signal.map { enabled =>
        if (enabled) "🔔 On" else "🔕 Off"
      },
      onClick --> Observer { _ =>
        val currentEnabled = notificationsEnabled.now()

        if (!currentEnabled) {
          // Request permission if needed
          NotificationCountdown.requestPermissionIfNeeded()

          // Check if we have permission
          if (NotificationCountdown.hasPermission) {
            notificationsEnabled.set(true)
            NotificationCountdown.startCountdownNotifications(
              $plan,
              timeStamps,
            )
          }
        }
        else {
          // Turn off notifications
          notificationsEnabled.set(false)
          NotificationCountdown.stopCountdownNotifications()
        }
      },
    )
  }

  def FullApp(
    javaClock: Clock,
  ) = {

    val appMode =
      if (dom.document.URL.contains("localhost")) AppMode.Local
      else AppMode.Production

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

    div(
      onMountCallback: context =>
        db.initializeOrResetStorage(),
      frontEndClock.clockElement,
      div(
        div(
          cls := ElementNames.BoxClass,
          idAttr := "container",

          // Scrolling wheel demo at the top
          Components.PlanElement(
            timeStamps,
            db,
            $plan,
            addingNewRoute,
            selectedStop.writer,
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
                                Observer { (rs: RouteSegment) =>
                                  val plan = $plan.now()
                                  val newPlan =
                                    plan
                                      .copy(l =
                                        plan.l.filterNot(_ == rs),
                                      )
                                  db.saveDailyPlanOnly(newPlan)
                                  $plan.set(newPlan)
                                  if (newPlan.l.isEmpty) {
                                    addingNewRoute.set {
                                      true
                                    }
                                  }
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
            val tripExpanded: Var[Boolean] = Var(false)

            val documentClickHandler
              : js.Function1[dom.MouseEvent, Unit] =
              (event: dom.MouseEvent) => {
                val target = event.target.asInstanceOf[dom.Element]
                val containerElement =
                  dom.document.querySelector(".trip-button-container")
                if (
                  containerElement != null && !containerElement
                    .contains(
                      target,
                    )
                ) {
                  tripExpanded.set(false)
                }
              }

            div(
              onMountCallback { _ =>
                dom.document.addEventListener("click",
                                              documentClickHandler,
                )
              },
              onUnmountCallback { _ =>
                dom.document.removeEventListener("click",
                                                 documentClickHandler,
                )
              },
              cls := "centered",
              div(
                cls := "trip-button-container",

                // Collapsed + Trip button
                button(
                  cls := "button floating-center-button button-fixed-width",
                  styleProp("opacity") <-- tripExpanded.signal.map(
                    expanded => if (expanded) "0" else "1",
                  ),
                  styleProp("pointer-events") <-- tripExpanded.signal
                    .map(expanded =>
                      if (expanded) "none" else "auto",
                    ),
                  span("+"),
                  onClick --> Observer { _ =>
                    tripExpanded.set(true)
                  },
                ),

                // Expanded buttons container
                div(
                  cls := "expanded-buttons-row",
                  styleProp("opacity") <-- tripExpanded.signal.map(
                    expanded => if (expanded) "1" else "0",
                  ),
                  styleProp("pointer-events") <-- tripExpanded.signal
                    .map(expanded =>
                      if (expanded) "auto" else "none",
                    ),

                  // New route button
                  button(
                    cls := "button button-fixed-width",
                    "New trip",
                    onClick --> Observer { _ =>
                      addingNewRoute.set(true)
                      setTimeout(300)(tripExpanded.set(false))
                    },
                  ),

                  // Return trip button
                  button(
                    cls := "button button-fixed-width",
                    "Return trip",
                    onClick --> Observer { _ =>
                      val plan = $plan.now()
                      val maybeLastSeg = plan.l.lastOption
                      maybeLastSeg.foreach { lastSeg =>
                        val (returnStart, returnEnd) =
                          returnTripEndpoints(lastSeg)
                        val maybeReturn =
                          rightLegOnRightRoute(
                            returnStart,
                            returnEnd,
                            plan,
                            lastSeg.end.t,
                          )
                        maybeReturn.foreach { newSeg =>
                          val updatedPlan =
                            plan.copy(l = plan.l :+ newSeg)
                          db.saveDailyPlanOnly(updatedPlan)
                          $plan.set(updatedPlan)
                          addingNewRoute.set(false)
                          setTimeout(300)(tripExpanded.set(false))
                        }
                      }
                    },
                  ),

                  // Notification toggle button
                  // TODO Re-enable if I can ever get it working reliably
                  // NotificationToggleButton(
                  //   $plan,
                  //   timeStamps,
                  //   buttonWidth,
                  // ),
                ),
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
      cls := "plan-segments",
      // Slidable content
      div(
        cls := "plan-segments_row",
        styleProp("transform") <-- offsetPx.signal.map(px =>
          if (px >= 0) s"translateX(-${px}px)"
          else s"translateX(${-px}px)",
        ),
        // Touch handlers for swipe-to-reveal (encapsulated)
        swipeModifier,
        div(
          cls := "plan-segments_left",
          div(
            s"${routeSegment.start.l.name} → ${routeSegment.end.l.name}",
          ),
          selectedValue --> segmentUpdater, // TODO Eventually this should be restored
          wheelElement,
        ),
      ),
    )

  }

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
  ) = {
    val isExpanded: Var[Boolean] = Var(false)

    // Add click handler to collapse when clicking outside
    val documentClickHandler: js.Function1[dom.MouseEvent, Unit] =
      (event: dom.MouseEvent) => {
        val target = event.target.asInstanceOf[dom.Element]
        val containerElement =
          dom.document.querySelector(".share-button-container")
        if (
          containerElement != null && !containerElement.contains(
            target,
          )
        ) {
          isExpanded.set(false)
        }
      }

    div(
      onMountCallback { ctx =>
        dom.document.addEventListener("click", documentClickHandler)
      },
      onUnmountCallback { _ =>
        dom.document.removeEventListener("click",
                                         documentClickHandler,
        )
      },
      child <-- $plan.map { plan =>
        if (plan.routeSegments.isEmpty)
          div()
        else {
          div(
            cls := "share-button-container",

            // Share button (visible when collapsed)
            button(
              cls := "button floating-center-button button-fixed-width",
              styleProp("opacity") <-- isExpanded.signal.map(
                expanded => if (expanded) "0" else "1",
              ),
              styleProp("pointer-events") <-- isExpanded.signal.map(
                expanded => if (expanded) "none" else "auto",
              ),
              span("Share"),
              onClick --> Observer { _ =>
                isExpanded.set(true)
              },
            ),

            // Expanded buttons container to guarantee separation
            div(
              cls := "expanded-buttons-row",
              styleProp("opacity") <-- isExpanded.signal.map(
                expanded => if (expanded) "1" else "0",
              ),
              styleProp("pointer-events") <-- isExpanded.signal.map(
                expanded => if (expanded) "auto" else "none",
              ),

              // Text button
              button(
                cls := "button button-fixed-width",
                "Text",
                onClick --> Observer { _ =>
                  val text = plan.plainTextRepresentation
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
                        js.Dynamic.literal(title = "Bus Schedule",
                                           text = text,
                        ),
                      )
                  }
                  else {
                    dom.window.navigator.clipboard.writeText(text)
                  }
                  setTimeout(300)(isExpanded.set(false))
                },
              ),

              // Link button
              button(
                cls := "button button-fixed-width",
                "Link",
                onClick --> Observer { _ =>
                  val url =
                    if (dom.document.URL.contains("localhost"))
                      s"http://localhost:8000/index.html?plan=${UrlEncoding.encode(plan)}"
                    else
                      s"https://rtabus.netlify.app/?plan=${UrlEncoding.encode(plan)}"
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
                        js.Dynamic.literal(title =
                                             "Bus Schedule Link",
                                           url = url,
                        ),
                      )
                  }
                  else {
                    dom.window.navigator.clipboard.writeText(url)
                  }
                  setTimeout(300)(isExpanded.set(false))
                },
              ),
            ),
          )
        }
      },
    )
  }

  /** Apply the rec-center/spencer special-casing requested for return
    * trips while keeping the opposite stop unchanged.
    */
  private[laminar] def returnTripEndpoints(
    lastSegment: RouteSegment,
  ): (Location, Location) = {
    val swappedStart = lastSegment.end.l
    val swappedEnd = lastSegment.start.l

    // TODO simplify these adjustments

    val adjustedStart =
      if (lastSegment.end.l == Location.RecCenter)
        Location.SpencerAndHighwayOneThirtyFive
      else if (
        lastSegment.end.l == Location.SpencerAndHighwayOneThirtyFive
      )
        Location.RecCenter
      else swappedStart

    val adjustedEnd =
      if (
        lastSegment.start.l == Location.SpencerAndHighwayOneThirtyFive
      )
        Location.RecCenter
      else if (lastSegment.start.l == Location.RecCenter)
        Location.SpencerAndHighwayOneThirtyFive
      else swappedEnd

    (adjustedStart, adjustedEnd)
  }

  def rightLegOnRightRoute(
    start: Location,
    end: Location,
    plan: Plan,
    pageLoadTime: WallTime,
  ): Option[RouteSegment] = {

    val defaultOrder =
      Seq(RTA.Southbound.fullSchedule, RTA.Northbound.fullSchedule)

    println(s"Finding route segments for start: $start, end: $end")

    val routesInPreferenceOrder =
      plan.l.lastOption match
        case Some(lastSeg)
            if lastSeg.route == RTA.Southbound.componentName =>
          Seq(RTA.Northbound.fullSchedule,
              RTA.Southbound.fullSchedule,
          )
        case Some(lastSeg)
            if lastSeg.route == RTA.Northbound.componentName =>
          Seq(RTA.Southbound.fullSchedule,
              RTA.Northbound.fullSchedule,
          )
        case _ => defaultOrder

    val routeSegmentsO =
      routesInPreferenceOrder.foldLeft(
        Option.empty[Seq[RouteSegment]],
      ) { case (acc, route) =>
        acc.foreach(segments => println(s"segments: $segments"))
        acc.orElse(route.segment(start, end))
      }

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
      h2("Select your origin and destination"),
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
                        "selected-starting-point"
                      case _ => ""
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
