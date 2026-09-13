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


/** Generate a default trip name based on current date.
  * Format: "Friday Mar 15"
  */
def defaultTripName(): String = {
  val now = new js.Date()
  val days = Seq("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
  val months = Seq("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  val dayName = days(now.getDay().toInt)
  val monthName = months(now.getMonth().toInt)
  val dayOfMonth = now.getDate().toInt
  s"$dayName $monthName $dayOfMonth"
}

case class SelectedStopInfo(
  busScheduleAtStop: BusScheduleAtStop,
  routeSegment: RouteSegment,
  context: StopContext)

object Components {

  /** Bell button for enabling bus countdown notifications. Only shown
    * on browsers that support reliable notification updates (not
    * Safari iOS).
    *
    * When clicked:
    *   - Requests notification permission if needed
    *   - Starts a countdown notification that updates every minute
    *   - Auto-stops when the bus arrives
    */
  def NotificationBellButton(
    $plan: Var[Plan],
  ) = {
    import scala.concurrent.ExecutionContext.Implicits.global

    println(
      s"NotificationBellButton: isSupported=${NotificationCountdown.isSupported}",
    )
    println(
      s"NotificationBellButton: hasNotificationAPI=${BrowserCapabilities.hasNotificationAPI}",
    )
    println(
      s"NotificationBellButton: hasServiceWorker=${BrowserCapabilities.hasServiceWorker}",
    )
    println(
      s"NotificationBellButton: isSafariIOS=${BrowserCapabilities.isSafariIOS}",
    )

    val notificationsEnabled: Var[Boolean] = Var(false)
    val permissionState: Var[String] = Var(
      if (BrowserCapabilities.hasNotificationAPI)
        dom.Notification.permission
      else "denied",
    )

    // Only render if the browser supports notification tag replacement
    if (!NotificationCountdown.isSupported) {
      println(
        "NotificationBellButton: NOT rendering - unsupported browser",
      )
      emptyNode
    }
    else {
      println("NotificationBellButton: rendering button")
      button(
        cls := "button bell-button",
        cls <-- notificationsEnabled.signal.map { enabled =>
          if (enabled) "bell-button-active" else ""
        },
        title <-- notificationsEnabled.signal.map { enabled =>
          if (enabled) "Click to stop bus alerts"
          else "Click to get bus arrival alerts"
        },
        child <-- notificationsEnabled.signal.map { enabled =>
          if (enabled)
            img(
              cls := "bell-icon bell-icon-active",
              src := "glyphicons/svg/individual-svg/glyphicons-basic-443-bell-ringing.svg",
              alt := "Notifications on",
              pointerEvents := "none", // Let clicks pass through to button
            )
          else
            img(
              cls := "bell-icon",
              src := "glyphicons/svg/individual-svg/glyphicons-basic-54-alarm.svg",
              alt := "Notifications off",
              pointerEvents := "none", // Let clicks pass through to button
            )
        },
        onPointerUp --> Observer { e =>
          println("NotificationBellButton: clicked!")
          val currentEnabled = notificationsEnabled.now()
          println(
            s"NotificationBellButton: currentEnabled=$currentEnabled",
          )

          if (!currentEnabled) {
            // Check permission state
            val permission = dom.Notification.permission
            println(s"NotificationBellButton: permission=$permission")
            permissionState.set(permission)

            if (permission == "denied") {
              // User previously denied - can't do anything
              dom.window.alert(
                "Notification permission was denied. Please enable notifications in your browser settings.",
              )
            }
            else if (permission == "default") {
              // Need to request permission
              println(
                "NotificationBellButton: requesting permission...",
              )
              dom.Notification.requestPermission { result =>
                println(
                  s"NotificationBellButton: permission result=$result",
                )
                permissionState.set(result)
                if (result == "granted") {
                  notificationsEnabled.set(true)
                  NotificationCountdown
                    .startCountdownNotifications($plan.now())
                }
              }
            }
            else {
              // Already granted
              println(
                "NotificationBellButton: already granted, starting notifications",
              )
              notificationsEnabled.set(true)
              NotificationCountdown
                .startCountdownNotifications($plan.now())
            }
          }
          else {
            // Turn off notifications
            println("NotificationBellButton: stopping notifications")
            notificationsEnabled.set(false)
            NotificationCountdown.stopCountdownNotifications()
          }
        },
      )
    }
  }

  // Legacy button kept for reference - uses emoji instead of icons
  def NotificationToggleButton(
    $plan: Var[Plan],
    timeStamps: Signal[WallTime],
    buttonWidth: Int,
  ) = {
    import scala.concurrent.ExecutionContext.Implicits.global

    val notificationsEnabled: Var[Boolean] = Var(false)

    if (!NotificationCountdown.isSupported) {
      emptyNode
    }
    else {
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
                $plan.now(),
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
      // Load from current SavedPlan if one exists, otherwise from "today"
      db.getDraft.map(_.plan)
        .orElse(db.getCurrentSavedPlan.map(_.plan))
        .orElse(db.getCurrentPlan)
        .getOrElse(Plan(Seq.empty)),
    )

    val addingNewRoute: Var[Boolean] = Var(
      $plan.now().routeSegments.isEmpty, // If no segments , assume we want to add more
    )

    val selectedStop: Var[Option[SelectedStopInfo]] =
      Var(None)

    div(

      frontEndClock.clockElement,
      OfflineStatus.element(db.storageProblem.signal.map(_.isEmpty)),
      child <-- db.storageProblem.signal.map(_.map(message => p(cls := "notification is-warning", role := "alert", message)).getOrElse(emptyNode)),
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
            () => java.time.LocalDate.now(javaClock).toString,
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
    today: () => String,
  ) =
    val isLocked: Var[Boolean] = Var(db.getScheduleLocked)
    val draftDate = Var(db.getDraft.map(_.date).getOrElse(today()))
    val dateLabel = timeStamps.combineWith(draftDate.signal).map { case (_, date) =>
      if (date == today()) "Today" else date
    }
    val tripNotice = Var(Option.empty[String])
    val undoPlan = Var(Option.empty[(Plan, String)])
    // Latest wall time, mirrored from the clock so reorder handlers can
    // resequence leg times against "now" without sampling a live signal.
    val latestTime: Var[WallTime] = Var(WallTime("00:00"))
    // Ref to the plan-segments container, so reorder can run a FLIP animation.
    var segmentsContainer: dom.Element = null
    // Track the current saved plan (None means it's a new unsaved plan or the daily plan)
    // Initialize from persistent storage to maintain state across page refreshes
    val currentSavedPlan: Var[Option[SavedPlan]] = Var(
      db.getDraft match {
        case Some(draft) => draft.savedPlanId.flatMap(db.getSavedPlan)
        case None => db.getCurrentSavedPlan
      },
    )
    
    // Track the original plan state when a saved plan is loaded
    // Used to detect unsaved changes (dirty state)
    val originalPlanOnLoad: Var[Option[Plan]] = Var(
      currentSavedPlan.now().map(_.plan),
    )

    // Track whether we should open the load trips view when entering stop selector
    val loadTripsMode: Var[Boolean] = Var(false)
    // Reactive state to track whether there are any saved plans
    val hasSavedPlans: Var[Boolean] = Var(
      db.listSavedPlans().nonEmpty || db.listPlanNames().nonEmpty,
    )
    
    // Derive dirty state: plan differs from original loaded state.
    // NOTE: RouteSegment case-class equality includes the ephemeral `id`
    // (see RouteSegment in common models). If a segment's id is ever
    // regenerated without a real content change (e.g. after decoding a plan
    // whose id was absent on the wire), this `!=` can report a false dirty
    // state. Compare on logical content (route/start/end) if that surfaces.
    val isDirty: Signal[Boolean] =
      $plan.signal.combineWith(originalPlanOnLoad.signal).map {
        case (current, Some(original)) => current != original
        case _ => false
      }
    
    // Pre-selected starting point for "Continue from last stop" action
    val preselectedStart: Var[Option[Location]] = Var(None)

    // Persist locked state changes to localStorage
    val persistLockedState =
      isLocked.signal.changes --> Observer[Boolean] { locked =>
        db.setScheduleLocked(locked)
      }

    // Persist current saved plan ID changes to localStorage
    val persistCurrentSavedPlan =
      currentSavedPlan.signal.changes --> Observer[
        Option[SavedPlan],
      ] {
        case Some(sp) => db.setCurrentSavedPlanId(sp.id)
        case None     => db.clearCurrentSavedPlanId()
      }

    // Derive whether we're in "load trips" mode (showing the saved trips list)
    val isLoadingTrips: Signal[Boolean] =
      loadTripsMode.signal.combineWith(addingNewRoute.signal).map {
        case (loadMode, adding) => loadMode // || adding
      }

    def useCurrentTripNow(): Unit =
      TripPlanning.useNow($plan.now(), latestTime.now()) match {
        case Left(message) => tripNotice.set(Some(message))
        case Right(updated) if updated == $plan.now() && draftDate.now() == today() =>
          tripNotice.set(Some("This trip already uses the next departures."))
        case Right(updated) =>
          undoPlan.set(Some(($plan.now(), draftDate.now())))
          draftDate.set(today())
          $plan.set(updated)
          isLocked.set(false)
          tripNotice.set(Some("Upcoming departures selected. Stopovers kept."))
      }

    div(
      cls := "plan-layout",
      $plan.signal.combineWith(currentSavedPlan.signal).combineWith(draftDate.signal) -->
        Observer[(Plan, Option[SavedPlan], String)] { case (plan, saved, date) => db.saveDraft(plan, saved.map(_.id), date) },
      div(cls := "trip-toolbar",
        display <-- $plan.signal.combineWith(isLoadingTrips).map { case (p, loading) => if (p.l.nonEmpty && !loading) "flex" else "none" },
        button(cls := "button", "Use this trip now", onClick --> Observer { _ => useCurrentTripNow() }),
        button(cls := "text-button", "Saved trips",
          display <-- hasSavedPlans.signal.map(has => if (has) "inline-flex" else "none"),
          onClick --> Observer { _ => loadTripsMode.set(true); addingNewRoute.set(true) }),
      ),
      p(cls := "muted trip-date", child.text <-- dateLabel.map(d => s"$d · Mountain Time"),
        display <-- $plan.signal.combineWith(isLoadingTrips).map { case (p, loading) => if (p.l.nonEmpty && !loading) "block" else "none" }),
      child <-- tripNotice.signal.map(_.map(message => p(cls := "trip-notice", role := "status", message)).getOrElse(emptyNode)),
      child <-- undoPlan.signal.map {
        case None => emptyNode
        case Some((previous, date)) => button(cls := "text-button", "Undo last trip change",
          onClick --> Observer { _ => draftDate.set(date); $plan.set(previous); undoPlan.set(None); tripNotice.set(None) })
      },
      persistLockedState,
      persistCurrentSavedPlan,
      timeStamps --> latestTime.writer,
      // Back button - only visible when loading trips
      div(
        cls := "action-buttons-container centered",
        display <-- isLoadingTrips.map(loading =>
          if (loading) "flex" else "none",
        ),
        button(
          cls := "button button-fixed-width",
          "← Back",
          onClick --> Observer { _ =>
            loadTripsMode.set(false)
            addingNewRoute.set(false)
          },
        ),
      ),
      // Plan segments container - hidden when loading trips (MOVED TO TOP)
      div(
        onMountCallback { ctx =>
          segmentsContainer = ctx.thisNode.ref
        },
        display <-- isLoadingTrips.map(loading =>
          if (loading) "none" else "block",
        ),
        children <-- $plan.signal
          .map(_.routePieces)
          .splitTransition(_.id) {
            case (_, routePiece, routePieceSignal, transition) =>
              // CRITICAL: Capture the currentSavedPlan when this element is created
              // to prevent saving to wrong plan if currentSavedPlan changes
              val capturedSavedPlan = currentSavedPlan.now()
              div(
                // Stable key so FLIP can match this element before/after a move.
                dataAttr("flip-key") := routePiece.id.toString,
                // Height enter/exit animates on add/remove of this keyed leg;
                // it's NOT on the inner element, so re-timing (a value change,
                // e.g. after a reorder) updates content in place without a
                // grow/shrink flash.
                transition.height,
                child <--
                  routePieceSignal
                    .map {
                      routePieceInner =>
                        div(
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
                                    s"Wait ${r.endTime.localTime.value - r.start.localTime.value} min",
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
                                    undoPlan.set(Some((plan, draftDate.now())))
                                    isLocked.set(false)
                                    val newPlan =
                                      plan
                                        .copy(l =
                                          plan.l.filterNot(_ == rs),
                                        )
                                    // Draft autosaves; named templates change only on Save.
                                    $plan.set(newPlan)
                                    if (newPlan.l.isEmpty) {
                                      addingNewRoute.set {
                                        true
                                      }
                                    }
                                  },
                                segmentUpdater = $plan.writer
                                  .contramap[RouteSegment] {
                                    segment =>
                                      isLocked.set(false)
                                      val plan = $plan.now()
                                      // Draft autosaves; named templates change only on Save.
                                      Plan(
                                        plan.l.map {
                                          case rs
                                              if rs.id == segment.id =>
                                            segment
                                          case rs =>
                                            rs
                                        },
                                      )
                                  },
                                // Append a new segment to the end of the plan
                                segmentAppender = $plan.writer
                                  .contramap[RouteSegment] {
                                    newSegment =>
                                      val plan = $plan.now()
                                      // Draft autosaves; named templates change only on Save.
                                      addingNewRoute.set(false)
                                      plan.copy(l = plan.l :+ newSegment)
                                  },
                                $isLocked = isLocked.signal,
                                $now = timeStamps,
                                $dateLabel = dateLabel,
                                segmentMover = Observer[Int] { dir =>
                                  isLocked.set(false)
                                  // FLIP: record positions, reorder, then on the
                                  // next frame (DOM settled) play old→new.
                                  val first =
                                    captureFlipRects(segmentsContainer)
                                  $plan.update(p =>
                                    moveSegment(p,
                                                rs.id,
                                                dir,
                                                latestTime.now(),
                                    ),
                                  )
                                  dom.window.requestAnimationFrame {
                                    (_: Double) =>
                                      playFlip(segmentsContainer, first)
                                  }
                                },
                                $movePosition = $plan.signal.map { p =>
                                  val segs = p.routeSegments
                                  val idx =
                                    segs.indexWhere(_.id == rs.id)
                                  (idx <= 0,
                                   idx < 0 || idx >= segs.size - 1,
                                  )
                                },
                              )

                          },
                        )
                    },
              )
          },
      ),
      div(
        cls := "add-new-route-section",
        child <-- addingNewRoute.signal.map {
          case false =>
            // Capture currentSavedPlan to prevent saving to wrong plan
            val capturedSavedPlan = currentSavedPlan.now()
            val tripExpanded: Var[Boolean] = Var(false)

            case class PendingReturnTrip(
              lastSegmentId: Long,
              options: ReturnTripOptions,
              adjustedAvailable: Boolean,
              originalAvailable: Boolean,
              generalMessage: Option[String] = None)

            val pendingReturnChoice: Var[Option[PendingReturnTrip]] =
              Var(None)

            def evaluatePendingReturn(
              lastSegment: RouteSegment,
              plan: Plan,
              options: ReturnTripOptions,
            ): PendingReturnTrip =
              val lastEndTime = lastSegment.end.t
              def hasReturnLeg(
                start: Location,
                end: Location,
              ) =
                rightLegOnRightRoute(
                  start,
                  end,
                  plan,
                  lastEndTime,
                ).nonEmpty

              val adjustedAvailable =
                hasReturnLeg(
                  options.adjustedStart,
                  options.adjustedEnd,
                )
              val originalAvailable =
                hasReturnLeg(
                  options.originalStart,
                  options.originalEnd,
                )

              PendingReturnTrip(
                lastSegment.id,
                options,
                adjustedAvailable,
                originalAvailable,
                generalMessage = None,
              )

            def attemptReturnTrip(
              start: Location,
              end: Location,
              expectedSegmentId: Option[Long],
            ): Unit =
              val currentPlan = $plan.now()
              val latestSegmentO = currentPlan.l.lastOption
              val isMatchingSegment =
                expectedSegmentId match
                  case Some(id) => latestSegmentO.exists(_.id == id)
                  case None     => latestSegmentO.isDefined

              if !isMatchingSegment then
                val updated =
                  pendingReturnChoice
                    .now()
                    .map(
                      _.copy(
                        adjustedAvailable = false,
                        originalAvailable = false,
                        generalMessage = Some(
                          "Your trip changed before we could add the return. Please try again.",
                        ),
                      ),
                    )
                pendingReturnChoice.set(updated)
              else
                latestSegmentO.foreach { lastSeg =>
                  val maybeReturnLeg =
                    rightLegOnRightRoute(
                      start,
                      end,
                      currentPlan,
                      lastSeg.end.t,
                    )
                  maybeReturnLeg match
                    case Some(newSeg) =>
                      val updatedPlan =
                        currentPlan.copy(l = currentPlan.l :+ newSeg)
                      // Draft autosaves; named templates change only on Save.
                      $plan.set(updatedPlan)
                      addingNewRoute.set(false)
                      pendingReturnChoice.set(None)
                      setTimeout(300)(tripExpanded.set(false))
                    case None =>
                      val defaultMessage =
                        "No matching return departure remains today. Your trip has not changed."
                      tripNotice.set(Some(defaultMessage))
                      val updated =
                        pendingReturnChoice
                          .now()
                          .map { pending =>
                            val isAdjustedChoice =
                              pending.options.adjustedStart == start &&
                                pending.options.adjustedEnd == end
                            val isOriginalChoice =
                              pending.options.originalStart == start &&
                                pending.options.originalEnd == end
                            val unavailableMessage =
                              if isAdjustedChoice then
                                s"No return trips leave ${pending.options.adjustedStart.name} right now."
                              else if isOriginalChoice then
                                s"No return trips leave ${pending.options.originalStart.name} right now."
                              else defaultMessage
                            pending.copy(
                              adjustedAvailable =
                                if isAdjustedChoice then false
                                else pending.adjustedAvailable,
                              originalAvailable =
                                if isOriginalChoice then false
                                else pending.originalAvailable,
                              generalMessage =
                                Some(unavailableMessage),
                            )
                          }
                      pendingReturnChoice.set(updated)
                }

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
                  // Hide when expanded to avoid overlap
                  display <-- tripExpanded.signal.map(expanded =>
                    if (expanded) "none" else "flex"
                  ),
                  span("Add another ride"),
                  onClick --> Observer { _ =>
                    tripExpanded.set(true)
                    isLocked.set(false) // Enter edit mode when adding trips
                  },
                ),

                // Expanded buttons container - vertical stack
                div(
                  cls := "expanded-buttons-column",
                  // Only render buttons when expanded to avoid overlap
                  display <-- tripExpanded.signal.map(expanded => 
                    if (expanded) "flex" else "none"
                  ),

                  // 1. Continue from last stop - pre-selects origin
                  button(
                    cls := "button trip-action-button",
                    div(
                      cls := "trip-action-content",
                      span(cls := "trip-action-icon", "→"),
                      div(
                        cls := "trip-action-text",
                        span(cls := "trip-action-title", "Continue from last stop"),
                        child <-- $plan.signal.map { plan =>
                          plan.l.lastOption match {
                            case Some(lastSeg) => 
                              span(cls := "trip-action-subtitle", s"From ${lastSeg.end.l.name}")
                            case None => emptyNode
                          }
                        },
                      ),
                    ),
                    onClick --> Observer { _ =>
                      val plan = $plan.now()
                      plan.l.lastOption.foreach { lastSeg =>
                        preselectedStart.set(Some(lastSeg.end.l))
                      }
                      addingNewRoute.set(true)
                      tripExpanded.set(false)
                    },
                  ),

                  // 2. Return to start - auto-creates full return segment
                  button(
                    cls := "button trip-action-button",
                    div(
                      cls := "trip-action-content",
                      span(cls := "trip-action-icon", "↩"),
                      div(
                        cls := "trip-action-text",
                        span(cls := "trip-action-title", "Return to start"),
                        child <-- $plan.signal.map { plan =>
                          (plan.l.lastOption, plan.l.headOption) match {
                            case (Some(lastSeg), Some(firstSeg)) => 
                              span(cls := "trip-action-subtitle", s"${lastSeg.end.l.name} → ${firstSeg.start.l.name}")
                            case _ => emptyNode
                          }
                        },
                      ),
                    ),
                    onClick --> Observer { _ =>
                      val plan = $plan.now()
                      (plan.l.lastOption, plan.l.headOption) match {
                        case (Some(lastSeg), Some(firstSeg)) =>
                          // Try to find a route from last stop back to first origin
                          val start = lastSeg.end.l
                          val end = firstSeg.start.l
                          attemptReturnTrip(start, end, Some(lastSeg.id))
                          tripExpanded.set(false)
                        case _ => ()
                      }
                    },
                  ),

                  // 3. New route - fresh start
                  button(
                    cls := "button trip-action-button button-outlined",
                    div(
                      cls := "trip-action-content",
                      span(cls := "trip-action-icon", "+"),
                      div(
                        cls := "trip-action-text",
                        span(cls := "trip-action-title", "New ride"),
                        span(cls := "trip-action-subtitle", "Pick a new starting point"),
                      ),
                    ),
                    onClick --> Observer { _ =>
                      preselectedStart.set(None)
                      addingNewRoute.set(true)
                      tripExpanded.set(false)
                    },
                  ),
                ),
              ),
              child <-- pendingReturnChoice.signal.map {
                case Some(pending) =>
                  val alternateLabel =
                    s"${pending.options.adjustedStart.name} → ${pending.options.adjustedEnd.name}"
                  val originalLabel =
                    s"${pending.options.originalStart.name} → ${pending.options.originalEnd.name}"
                  val alternateButton =
                    Option.when(pending.adjustedAvailable)(
                      button(
                        cls := "button return-trip-choice_button",
                        alternateLabel,
                        onClick --> Observer { _ =>
                          attemptReturnTrip(
                            pending.options.adjustedStart,
                            pending.options.adjustedEnd,
                            Some(pending.lastSegmentId),
                          )
                        },
                      ),
                    )
                  val originalButton =
                    Option.when(pending.originalAvailable)(
                      button(
                        cls :=
                          "button button-outlined return-trip-choice_button",
                        originalLabel,
                        onClick --> Observer { _ =>
                          attemptReturnTrip(
                            pending.options.originalStart,
                            pending.options.originalEnd,
                            Some(pending.lastSegmentId),
                          )
                        },
                      ),
                    )
                  val warningMessages =
                    pending.generalMessage match
                      case Some(message) => List(message)
                      case None =>
                        List(
                          Option.when(!pending.adjustedAvailable)(
                            s"No return trips leave ${pending.options.adjustedStart.name} right now.",
                          ),
                          Option.when(!pending.originalAvailable)(
                            s"No return trips leave ${pending.options.originalStart.name} right now.",
                          ),
                        ).flatten
                  val warningsNode =
                    if warningMessages.nonEmpty then
                      div(
                        cls := "notification is-warning return-trip-choice_warning",
                        p(warningMessages.mkString(" ")),
                      )
                    else emptyNode
                  val alternateButtonNode =
                    alternateButton.getOrElse(emptyNode)
                  val originalButtonNode =
                    originalButton.getOrElse(emptyNode)
                  val cancelButton =
                    button(
                      cls := "button button-ghost return-trip-choice_button",
                      "Cancel",
                      onClick --> Observer { _ =>
                        pendingReturnChoice.set(None)
                      },
                    )
                  div(
                    cls := "return-trip-choice",
                    h3("Choose your return stop"),
                    p(
                      "The alternate stop is usually faster, but you can keep your original stop if you prefer.",
                    ),
                    warningsNode,
                    div(
                      cls := "return-trip-choice_buttons",
                      alternateButtonNode,
                      originalButtonNode,
                      cancelButton,
                    ),
                  )
                case None => emptyNode
              },
            )
          case true =>
            // Capture preselected start and clear it for next time
            val initialStart = preselectedStart.now()
            preselectedStart.set(None)
            div(
              StopSelector(
                CompleteStopList.values,
                $plan,
                db,
                timeStamps,
                addingNewRoute,
                currentSavedPlan,
                isLocked,
                loadTripsMode,
                hasSavedPlans,
                originalPlanOnLoad,
                initialStart,
                () => draftDate.set(today()),
              ),
            )
        },
      ),
      // Bottom controls - fixed at bottom for thumb-friendly access
      div(
        cls := "bottom-controls-wrapper",
        display <-- isLoadingTrips.map(loading =>
          if (loading) "none" else "block",
        ),
        unifiedBottomBar(
          $plan,
          currentSavedPlan,
          isLocked,
          db,
          hasSavedPlans,
          isDirty,
          originalPlanOnLoad,
          addingNewRoute,
          loadTripsMode,
          () => draftDate.set(today()),
        ),
      ),
    )

  /** Unified bottom bar with trip name menu and contextual actions.
    * 
    * Layout:
    * - When viewing (locked): [Name ▼] opens menu with Edit, Share, New, Load, etc.
    * - When editing (unlocked): [Name input] [Save] [Discard]
    * - Dirty indicator shown when there are unsaved changes
    */
  def unifiedBottomBar(
    $plan: Var[Plan],
    $currentSavedPlan: Var[Option[SavedPlan]],
    isLocked: Var[Boolean],
    db: Persistence,
    hasSavedPlans: Var[Boolean],
    $isDirty: Signal[Boolean],
    originalPlanOnLoad: Var[Option[Plan]],
    addingNewRoute: Var[Boolean],
    loadTripsMode: Var[Boolean],
    resetDate: () => Unit,
  ) = {
    val menuOpen: Var[Boolean] = Var(false)
    val editingName: Var[String] = Var("")
    val saveMode: Var[Boolean] = Var(false) // For "Unsaved Trip" -> save dialog

    // Derive display name
    val displayName: Signal[String] = 
      $currentSavedPlan.signal.map {
        case Some(sp) => sp.displayName
        case None => "Unsaved Trip"
      }

    // Derive dirty state
    val isDirtyNow: Signal[Boolean] = 
      $currentSavedPlan.signal.combineWith($plan.signal).combineWith(originalPlanOnLoad.signal).map { _ =>
        val savedPlanO = $currentSavedPlan.now()
        val current = $plan.now()
        val originalO = originalPlanOnLoad.now()
        savedPlanO.isDefined && originalO.exists(_ != current)
      }

    def doSaveNew(): Unit = {
      val suggestedName = defaultTripName()
      val enteredName = editingName.now().trim.take(20)
      val name = if (enteredName.nonEmpty) enteredName else suggestedName
      val plan = $plan.now()
      val newSavedPlan = SavedPlan.create(plan, name)
      if (!db.saveSavedPlan(newSavedPlan)) return
      $currentSavedPlan.set(Some(newSavedPlan))
      originalPlanOnLoad.set(Some(plan))
      hasSavedPlans.set(true)
      isLocked.set(true)
      editingName.set("")
      saveMode.set(false)
    }

    def doSaveExisting(): Unit = {
      $currentSavedPlan.now().foreach { sp =>
        val finalName = editingName.now().trim
        val nameToUse = if (finalName.nonEmpty) finalName else sp.displayName
        val updatedPlan = sp.withName(nameToUse).withPlan($plan.now())
        if (db.saveSavedPlan(updatedPlan)) {
          $currentSavedPlan.set(Some(updatedPlan))
          originalPlanOnLoad.set(Some($plan.now()))
          isLocked.set(true)
          editingName.set("")
        }
      }
    }

    def discardChanges(): Unit = {
      $currentSavedPlan.now().foreach { sp =>
        db.getSavedPlan(sp.id).foreach { freshPlan =>
          $plan.set(freshPlan.plan)
          originalPlanOnLoad.set(Some(freshPlan.plan))
          $currentSavedPlan.set(Some(freshPlan))
          isLocked.set(true)
          editingName.set("")
        }
      }
    }

    def startNewTrip(): Unit = {
      resetDate()
      val emptyPlan = Plan(Seq.empty)
      db.saveDailyPlanOnly(emptyPlan)
      $plan.set(emptyPlan)
      $currentSavedPlan.set(None)
      originalPlanOnLoad.set(None)
      isLocked.set(false)
      addingNewRoute.set(true)
    }

    // Build menu items dynamically
    // Combine signals to trigger updates, read values inside
    val menuItems: Signal[Seq[BottomSheet.MenuItem]] =
      $currentSavedPlan.signal
        .combineWith(isLocked.signal)
        .combineWith(isDirtyNow)
        .combineWith(hasSavedPlans.signal)
        .combineWith($plan.signal)
        .map { _ =>
          // Read current values - Vars have .now(), Signals need observe pattern
          val savedPlanO = $currentSavedPlan.now()
          val locked = isLocked.now()
          val hasPlans = hasSavedPlans.now()
          val hasSegments = $plan.now().routeSegments.nonEmpty
          // For dirty, we check it directly since we have the signals
          val dirty = savedPlanO.exists { _ =>
            originalPlanOnLoad.now().exists(_ != $plan.now())
          }
          val isSaved = savedPlanO.isDefined
          
          Seq(
            // Edit times - only when locked and has segments
            BottomSheet.MenuItem(
              icon = "✏️",
              label = "Rename trip",
              onClick = () => isLocked.set(false),
              hidden = !locked || !hasSegments,
            ),
            // Save changes - only when dirty and saved and has segments
            BottomSheet.MenuItem(
              icon = "💾",
              label = "Save changes",
              onClick = () => {
                savedPlanO.foreach { sp =>
                  val updatedPlan = sp.withPlan($plan.now())
                  if (db.saveSavedPlan(updatedPlan)) {
                    $currentSavedPlan.set(Some(updatedPlan))
                    originalPlanOnLoad.set(Some($plan.now()))
                  }
                }
              },
              hidden = !dirty || !isSaved || !hasSegments,
            ),
            // Save as new trip - for unsaved trips with segments
            BottomSheet.MenuItem(
              icon = "💾",
              label = "Save trip",
              onClick = () => saveMode.set(true),
              hidden = isSaved || !hasSegments,
            ),
            // Share as text - only with segments
            BottomSheet.MenuItem(
              icon = "📝",
              label = "Share as text",
              onClick = () => {
                val text = $plan.now().plainTextRepresentation
                if (js.typeOf(dom.window.navigator.asInstanceOf[js.Dynamic].share) != "undefined") {
                  dom.window.navigator.asInstanceOf[js.Dynamic].share(
                    js.Dynamic.literal(title = "Bus Schedule", text = text)
                  )
                } else {
                  dom.window.navigator.clipboard.writeText(text)
                }
              },
              hidden = !hasSegments,
            ),
            // Share as link - only with segments
            BottomSheet.MenuItem(
              icon = "🔗",
              label = "Share link",
              onClick = () => {
                val plan = $plan.now()
                val url = s"${dom.window.location.origin}/?plan=${UrlEncoding.encode(plan)}"
                if (js.typeOf(dom.window.navigator.asInstanceOf[js.Dynamic].share) != "undefined") {
                  dom.window.navigator.asInstanceOf[js.Dynamic].share(
                    js.Dynamic.literal(title = "Bus Schedule", url = url)
                  )
                } else {
                  dom.window.navigator.clipboard.writeText(url)
                }
              },
              hidden = !hasSegments,
            ),
            // Notifications - only with segments
            BottomSheet.MenuItem(
              icon = "🔔",
              label = "Set departure alert",
              onClick = () => {
                // Trigger notification setup
                if (NotificationCountdown.isSupported) {
                  if (dom.Notification.permission == "granted") {
                    NotificationCountdown.startCountdownNotifications($plan.now())
                  } else if (dom.Notification.permission != "denied") {
                    dom.Notification.requestPermission { result =>
                      if (result == "granted") {
                        NotificationCountdown.startCountdownNotifications($plan.now())
                      }
                    }
                  }
                }
              },
              hidden = !NotificationCountdown.isSupported || !hasSegments,
            ),
            // New trip - only with segments (otherwise use stop selector directly)
            BottomSheet.MenuItem(
              icon = "➕",
              label = "New trip",
              onClick = () => startNewTrip(),
              hidden = !hasSegments,
            ),
            // Load trip - always visible when there are saved plans
            BottomSheet.MenuItem(
              icon = "📂",
              label = "Load saved trip",
              onClick = () => {
                loadTripsMode.set(true)
                addingNewRoute.set(true)
              },
              hidden = !hasPlans,
            ),
            // Delete - only for saved trips with segments
            BottomSheet.MenuItem(
              icon = "🗑️",
              label = "Delete trip",
              onClick = () => {
                savedPlanO.foreach { sp =>
                  db.deleteSavedPlan(sp.id)
                  $currentSavedPlan.set(None)
                  db.clearCurrentSavedPlanId()
                  hasSavedPlans.set(db.listSavedPlans().nonEmpty)
                }
              },
              hidden = !isSaved || !hasSegments,
            ),
          )
      }

    div(
      // Show when plan has segments OR user has saved plans to load
      display <-- $plan.signal.combineWith(hasSavedPlans.signal).map { _ =>
        val hasSegments = $plan.now().routeSegments.nonEmpty
        val hasPlans = hasSavedPlans.now()
        if (hasSegments || hasPlans) "block" else "none"
      },
      
      // Bottom sheet menu
      BottomSheet(menuOpen, "Trip Options", menuItems),
      
      // Main bar - two rows: optional edit row on top, name+arrow row always at bottom
      div(
        cls := "unified-bottom-bar",
        
        // Top row: Edit controls (only visible in save/edit mode)
        child <-- isLocked.signal
          .combineWith(saveMode.signal)
          .combineWith($currentSavedPlan.signal)
          .map { _ =>
            val locked = isLocked.now()
            val saving = saveMode.now()
            val savedPlanO = $currentSavedPlan.now()
            val isSaved = savedPlanO.isDefined
            
            if (saving) {
              // Save dialog for new trip
              val suggestedName = defaultTripName()
              div(
                cls := "bottom-bar-edit-row",
                input(
                  cls := "bottom-bar-name-input",
                  typ := "text",
                  placeholder := suggestedName,
                  maxLength := 20,
                  onInput.mapToValue --> editingName.writer,
                  onKeyDown --> Observer[dom.KeyboardEvent] { evt =>
                    if (evt.key == "Enter") doSaveNew()
                    else if (evt.key == "Escape") saveMode.set(false)
                  },
                  TouchControls.onTouchStart.stopPropagation --> Observer.empty,
                  TouchControls.onTouchMove.stopPropagation --> Observer.empty,
                ),
                button(
                  cls := "button bottom-bar-save-btn",
                  "Save",
                  onClick --> Observer { _ => doSaveNew() },
                ),
                button(
                  cls := "button button-outlined bottom-bar-cancel-btn",
                  "✕",
                  onClick --> Observer { _ => saveMode.set(false) },
                ),
              )
            } else if (!locked && isSaved) {
              // Edit mode for saved trip - show input + Save/Discard
              val sp = savedPlanO.get
              div(
                cls := "bottom-bar-edit-row",
                input(
                  cls := "bottom-bar-name-input",
                  typ := "text",
                  placeholder := "Trip name",
                  defaultValue := sp.name.getOrElse(""),
                  maxLength := 30,
                  onMountCallback { ctx =>
                    editingName.set(sp.name.getOrElse(""))
                  },
                  onInput.mapToValue --> editingName.writer,
                  TouchControls.onTouchStart.stopPropagation --> Observer.empty,
                  TouchControls.onTouchMove.stopPropagation --> Observer.empty,
                ),
                button(
                  cls := "button bottom-bar-save-btn",
                  "Save",
                  onClick --> Observer { _ => doSaveExisting() },
                ),
                button(
                  cls := "button button-outlined bottom-bar-discard-btn",
                  "Discard",
                  onClick --> Observer { _ => discardChanges() },
                ),
              )
            } else {
              // View mode - no edit row needed
              emptyNode
            }
          },
        
        // Bottom row: Name + arrow (ALWAYS visible, arrow always in same spot)
        div(
          cls := "bottom-bar-main-row",
          // Hide when menu is open
          display <-- menuOpen.signal.map(open => if (open) "none" else "flex"),
          
          // Left side: name display with dirty indicator
          div(
            cls := "bottom-bar-name-area",
            // While the plan is unsaved and has segments, the name doubles as a
            // one-tap "save this trip" target (the menu still offers "Save
            // trip"). stopPropagation so the tap can't start a parent gesture.
            child <-- $currentSavedPlan.signal
              .combineWith($plan.signal)
              .map { case (savedO, plan) =>
                val name = savedO.map(_.displayName).getOrElse("Unsaved Trip")
                if (savedO.isEmpty && plan.routeSegments.nonEmpty)
                  span(
                    cls := "bottom-bar-name bottom-bar-name--tappable",
                    title := "Tap to save this trip",
                    name,
                    onClick.stopPropagation --> Observer { _ =>
                      saveMode.set(true)
                    },
                  )
                else
                  span(cls := "bottom-bar-name", name)
              },
            // Dirty indicator
            child <-- isDirtyNow.map { dirty =>
              if (dirty) span(cls := "bottom-bar-dirty", "•")
              else emptyNode
            },
          ),
          
          // Right side: Arrow button (ALWAYS here, rock solid position)
          button(
            cls := "bottom-bar-menu-btn",
            "Trip options",
            onClick --> Observer { _ => menuOpen.set(true) },
          ),
        ),
      ),
    )
  }

  def RouteLegElement(
    routeSegment: RouteSegment,
    addingNewRoute: Var[Boolean],
    scheduleSelector: Observer[Option[SelectedStopInfo]],
    legDeleter: Observer[RouteSegment],
    segmentUpdater: Observer[RouteSegment],
    segmentAppender: Observer[RouteSegment],
    $isLocked: Signal[Boolean] = Val(false),
    $now: Signal[WallTime] = Val(WallTime("00:00")),
    $dateLabel: Signal[String] = Val("Today"),
    segmentMover: Observer[Int] = Observer.empty,
    $movePosition: Signal[(Boolean, Boolean)] = Val((true, true)),
  ) = DepartureCard(routeSegment, $now, $dateLabel, segmentUpdater,
    legDeleter, segmentMover, $movePosition)

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

  /** Helper to save a plan - if we have a saved plan loaded (with
    * UUID), save only to the SavedPlan storage. Otherwise save to
    * "today" for unsaved work-in-progress.
    */
  private def savePlanWithSavedPlan(
    db: Persistence,
    plan: Plan,
    savedPlanO: Option[SavedPlan],
  ): Unit =
    savedPlanO match {
      case Some(sp) =>
        // SavedPlan is loaded - only write to SavedPlan storage
        db.saveSavedPlan(sp.withPlan(plan))
      case None =>
        // No SavedPlan - write to "today" for unsaved work
        db.saveDailyPlanOnly(plan)
    }

  private[laminar] case class ReturnTripOptions(
    originalStart: Location,
    originalEnd: Location,
    adjustedStart: Location,
    adjustedEnd: Location) {
    val startAdjusted = originalStart != adjustedStart
    val endAdjusted = originalEnd != adjustedEnd
    val hasAdjustments = startAdjusted || endAdjusted
  }

  /** Apply the rec-center/spencer special-casing requested for return
    * trips while keeping the opposite stop unchanged, but also keep
    * the original endpoints so we can prompt the user when we make a
    * change.
    */
  private[laminar] def returnTripEndpoints(
    lastSegment: RouteSegment,
  ): ReturnTripOptions = {
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

    ReturnTripOptions(
      originalStart = swappedStart,
      originalEnd = swappedEnd,
      adjustedStart = adjustedStart,
      adjustedEnd = adjustedEnd,
    )
  }

  /** Rebuild a plan's leg times in list order: each leg snaps to its earliest
    * valid departure after the previous leg arrives (the first leg after `now`),
    * reusing [[rightLegOnRightRoute]]. Keeps a reordered trip a real, rideable
    * itinerary. A leg that can't be resolved in the new order keeps its time. */
  def resequencePlan(
    plan: Plan,
    now: WallTime,
  ): Plan =
    plan.routeSegments.foldLeft(Plan(Seq.empty)) { (acc, seg) =>
      rightLegOnRightRoute(seg.start.l, seg.end.l, acc, now) match
        // Keep the leg's original id so splitTransition reconciles the existing
        // element into its new slot instead of rebuilding the whole list.
        case Some(fixed) => acc.copy(l = acc.l :+ fixed.withId(seg.id))
        case None        => acc.copy(l = acc.l :+ seg)
    }

  /** Move the segment with `id` one slot in `direction` (-1 up, +1 down), then
    * re-time the whole chain. No-op when already at that end. */
  def moveSegment(
    plan: Plan,
    id: Long,
    direction: Int,
    now: WallTime,
  ): Plan =
    val segs   = plan.routeSegments
    val idx    = segs.indexWhere(_.id == id)
    val target = idx + direction
    if idx < 0 || target < 0 || target >= segs.size then plan
    else
      val buf   = segs.toBuffer
      val moved = buf.remove(idx)
      buf.insert(target, moved)
      resequencePlan(Plan(buf.toSeq), now)

  /** FLIP animation for reordering. Capture each keyed child's position (by its
    * `data-flip-key`) BEFORE the plan changes... */
  private def captureFlipRects(
    container: dom.Element,
  ): Map[String, (Double, Double)] =
    if container == null then Map.empty
    else
      val kids = container.children
      val b    = Map.newBuilder[String, (Double, Double)]
      var i    = 0
      while i < kids.length do
        val el  = kids(i).asInstanceOf[dom.HTMLElement]
        val key = el.getAttribute("data-flip-key")
        if key != null then
          val r = el.getBoundingClientRect()
          b += key -> (r.left, r.top)
        i += 1
      b.result()

  /** ...then, once the DOM has settled in its new order, invert each moved child
    * back to its old spot and spring it home. The vertical offset is driven by
    * an Animus spring (`Var[Double].signal.spring`) applied to `transform`, so
    * the motion has natural spring physics and never touches the height
    * transition or the swipe transform. */
  private def playFlip(
    container: dom.Element,
    first: Map[String, (Double, Double)],
  ): Unit =
    if container != null then
      val kids = container.children
      var i    = 0
      while i < kids.length do
        val el  = kids(i).asInstanceOf[dom.HTMLElement]
        val key = el.getAttribute("data-flip-key")
        (if key == null then None else first.get(key)).foreach {
          case (_, fTop) =>
            val dy = fTop - el.getBoundingClientRect().top // old − new (vertical)
            if Math.abs(dy) > 0.5 then
              // Spring starts at the old offset (its initial value) and, once we
              // set the target to 0, springs the element home to its new slot.
              val offset = Var(dy)
              val owner  = new com.raquo.airstream.ownership.ManualOwner
              offset.signal.spring.foreach { v =>
                el.style.transform =
                  if Math.abs(v) < 0.3 then "" else s"translateY(${v}px)"
              }(owner)
              offset.set(0.0)
              // Tear down once the spring has settled.
              setTimeout(1500)(owner.killSubscriptions())
        }
        i += 1

  def rightLegOnRightRoute(
    start: Location,
    end: Location,
    plan: Plan,
    pageLoadTime: WallTime,
  ): Option[RouteSegment] = {

    val defaultOrder =
      Seq(RTA.Southbound.fullSchedule, RTA.Northbound.fullSchedule)

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

    val cutoff = plan.l.lastOption.map(_.end.t).getOrElse(pageLoadTime)
    routesInPreferenceOrder.flatMap { route =>
      route.segment(start, end).flatMap { segments =>
        segments.filter(s => s.start.t.isAfter(cutoff) && !s.end.t.isBefore(s.start.t))
          .sortBy(_.start.t.localTime.value).headOption
      }
    }.headOption

  }

  sealed trait StopSelectorMode
  object StopSelectorMode {
    case object SelectStop extends StopSelectorMode
    case object LoadSavedTrip extends StopSelectorMode
  }

  def SavedTripsSelector(
    db: Persistence,
    $plan: Var[Plan],
    addingNewRoute: Var[Boolean],
    currentSavedPlan: Var[Option[SavedPlan]],
    isLocked: Var[Boolean],
    loadTripsMode: Var[Boolean],
    hasSavedPlans: Var[Boolean],
    originalPlanOnLoad: Var[Option[Plan]],
    now: Signal[WallTime],
    resetDate: () => Unit,
  ) = {
    val latestTime = Var(WallTime("00:00"))
    val reuseError = Var(Option.empty[String])
    // Load saved plans using the new UUID-based system, with fallback to legacy name-based system
    val $savedTripsVar: Var[Seq[(SavedPlan, Int)]] = Var(Seq.empty)

    def loadSavedTrips(): Unit = {
      $savedTripsVar.set(Seq.empty)
      // First, try to load from new UUID-based storage
      val savedPlans = db.listSavedPlans()
      // Also load legacy name-based plans and convert them
      val legacyNames = db.listPlanNames()
      val legacyPlans = legacyNames.flatMap { name =>
        db.getPlanByName(name)
          .map { plan =>
            // Check if we already have this in new format (by name match)
            if (!savedPlans.exists(sp => sp.name.contains(name))) {
              // Migrate to new format
              val newSavedPlan = SavedPlan.create(plan, name)
              Option.when(db.saveSavedPlan(newSavedPlan))(newSavedPlan)
            }
            else None
          }
          .flatten
      }
      // Purge all legacy plans now that migration is complete
      if (legacyNames.nonEmpty && legacyNames.forall(name => db.listSavedPlans().exists(_.name.contains(name)))) {
        db.purgeLegacyNamedPlans()
      }
      val allPlans = (savedPlans ++ legacyPlans).distinctBy(_.id)
      allPlans.zipWithIndex.foreach { case (sp, idx) =>
        setTimeout(idx * 30) {
          $savedTripsVar.update(_ :+ (sp, idx))
        }
      }
    }

    div(
      cls := "saved-trips-selector",
      now --> latestTime.writer,
      child <-- reuseError.signal.map(_.map(message => p(role := "alert", cls := "trip-notice", message)).getOrElse(emptyNode)),
      onMountCallback { _ =>
        loadSavedTrips()
      },
      h2("Load a saved trip"),
      child <-- $savedTripsVar.signal.map { trips =>
        if (
          trips.isEmpty && db.listSavedPlans().isEmpty && db
            .listPlanNames()
            .isEmpty
        )
          div(
            cls := "no-saved-trips",
            p("No saved trips yet."),
            p("Create a trip and save it to see it here."),
          )
        else
          emptyNode
      },
      div(
        children <-- $savedTripsVar.signal.splitTransition(_._1.id) {
          case (_, (savedPlan, _), _, transition) =>
            // Outer wrapper handles height animation with overflow:hidden
            // so the card's padding/margin/border collapse smoothly
            div(
              transition.height,
              cls := "saved-trip-card-wrapper",
              div(
                cls := "saved-trip-card",
                div(
                  cls := "saved-trip-card-header",
                  span(cls := "saved-trip-name",
                       savedPlan.displayName,
                  ),
                  button(
                    cls := "saved-trip-delete",
                    "✕",
                    onClick --> Observer { _ =>
                      // Check if we're deleting the currently active plan
                      val isDeletingCurrentPlan =
                        currentSavedPlan
                          .now()
                          .exists(_.id == savedPlan.id)

                      // Delete the plan from storage
                      db.deleteSavedPlan(savedPlan.id)

                      // If we deleted the current plan, clear the reference
                      // This reverts to the default "Current Plan" state
                      if (isDeletingCurrentPlan) {
                        currentSavedPlan.set(None)
                        db.clearCurrentSavedPlanId()
                      }

                      // Remove from UI list
                      $savedTripsVar.update(
                        _.filterNot(_._1.id == savedPlan.id),
                      )

                      // Update hasSavedPlans if this was the last plan
                      val remainingPlansExist =
                        db.listSavedPlans().nonEmpty || db
                          .listPlanNames()
                          .nonEmpty
                      hasSavedPlans.set(remainingPlansExist)
                    },
                  ),
                ),
                div(
                  cls := "saved-trip-segments",
                  savedPlan.plan.routeSegments.map { segment =>
                    div(
                      cls := "saved-trip-segment",
                      span(
                        cls := "saved-trip-segment-route",
                        s"${segment.start.l.name} → ${segment.end.l.name}",
                      ),
                      span(
                        cls := "saved-trip-segment-times",
                        s"${segment.start.t.toDumbAmericanString} - ${segment.end.t.toDumbAmericanString}",
                      ),
                    )
                  },
                ),
                button(
                  cls := "button primary-action",
                  "Use this trip now",
                  onClick --> Observer { _ =>
                    TripPlanning.useNow(savedPlan.plan, latestTime.now()) match {
                      case Left(message) => reuseError.set(Some(message))
                      case Right(updated) =>
                        resetDate()
                        currentSavedPlan.set(Some(savedPlan))
                        originalPlanOnLoad.set(Some(savedPlan.plan))
                        $plan.set(updated)
                        loadTripsMode.set(false)
                        addingNewRoute.set(false)
                        isLocked.set(updated == savedPlan.plan)
                    }
                  },
                ),
                button(
                  cls := "button saved-trip-load-button",
                  "Load saved times",
                  onClick --> Observer { _ =>
                    // CRITICAL: Set currentSavedPlan BEFORE $plan to ensure observers
                    // use the correct plan ID if they fire during the transition
                    resetDate()
                    currentSavedPlan.set(Some(savedPlan))
                    $plan.set(savedPlan.plan)
                    // Track original state for dirty detection
                    originalPlanOnLoad.set(Some(savedPlan.plan))
                    loadTripsMode.set(false)
                    addingNewRoute.set(false)
                    isLocked.set(true)
                  },
                ),
              ),
            )
        },
      ),
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
    currentSavedPlan: Var[Option[SavedPlan]],
    isLocked: Var[Boolean],
    loadTripsMode: Var[Boolean],
    hasSavedPlans: Var[Boolean],
    originalPlanOnLoad: Var[Option[Plan]],
    initialStartingPoint: Option[Location] = None,
    resetDate: () => Unit = () => (),
  ) =
    val latestTime = Var(WallTime("00:00"))
    div(
      $now --> latestTime.writer,
      child <-- loadTripsMode.signal.map {
        case true => SavedTripsSelector(db, $plan, addingNewRoute, currentSavedPlan,
          isLocked, loadTripsMode, hasSavedPlans, originalPlanOnLoad, $now, resetDate)
        case false => StopPicker(locations, db, initialStartingPoint, $now,
          (start, end, now) => rightLegOnRightRoute(start, end, $plan.now(), now).isDefined,
          (start, end) => {
            rightLegOnRightRoute(start, end, $plan.now(), latestTime.now()).foreach { leg =>
              if ($plan.now().l.isEmpty) resetDate()
              db.rememberStop(start)
              db.rememberStop(end)
              $plan.update(p => p.copy(l = p.l :+ leg))
              addingNewRoute.set(false)
            }
          },
          hasSavedPlans.signal,
          () => loadTripsMode.set(true),
          () => addingNewRoute.set(false),
          $plan.now().l.nonEmpty,
        )
      },
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
