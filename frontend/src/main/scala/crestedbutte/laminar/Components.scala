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

// Note: SavedTrip is now replaced by SavedPlan in models.scala

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
      db.getCurrentSavedPlan
        .map(_.plan)
        .orElse(db.getCurrentPlan)
        .getOrElse(Plan(Seq.empty)),
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
    val isLocked: Var[Boolean] = Var(db.getScheduleLocked)
    // Track the current saved plan (None means it's a new unsaved plan or the daily plan)
    // Initialize from persistent storage to maintain state across page refreshes
    val currentSavedPlan: Var[Option[SavedPlan]] = Var(
      db.getCurrentSavedPlan,
    )
    
    // Track the original plan state when a saved plan is loaded
    // Used to detect unsaved changes (dirty state)
    val originalPlanOnLoad: Var[Option[Plan]] = Var(
      db.getCurrentSavedPlan.map(_.plan),
    )

    // Track whether we should open the load trips view when entering stop selector
    val loadTripsMode: Var[Boolean] = Var(false)
    // Signal to trigger focusing the plan name input when saving
    val focusPlanNameInput: Var[Boolean] = Var(false)
    // Reactive state to track whether there are any saved plans
    val hasSavedPlans: Var[Boolean] = Var(
      db.listSavedPlans().nonEmpty || db.listPlanNames().nonEmpty,
    )
    
    // Derive dirty state: plan differs from original loaded state
    val isDirty: Signal[Boolean] = 
      $plan.signal.combineWith(originalPlanOnLoad.signal).map {
        case (current, Some(original)) => current != original
        case _ => false
      }
    
    // Shared state for save mode - triggered from name area, handled in copyButtons
    val saveExpanded: Var[Boolean] = Var(false)

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

    div(
      cls := "plan-layout",
      persistLockedState,
      persistCurrentSavedPlan,
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
                                    // Don't auto-save - wait for explicit Save
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
                                      val plan = $plan.now()
                                      // Don't auto-save - wait for explicit Save
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
                                      // Don't auto-save - wait for explicit Save
                                      addingNewRoute.set(false)
                                      plan.copy(l = plan.l :+ newSegment)
                                  },
                                $isLocked = isLocked.signal,
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
                      // Don't auto-save - wait for explicit Save
                      $plan.set(updatedPlan)
                      addingNewRoute.set(false)
                      pendingReturnChoice.set(None)
                      setTimeout(300)(tripExpanded.set(false))
                    case None =>
                      val defaultMessage =
                        "Couldn't find a matching return route from that stop."
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
                  cls := "button floating-center-button button-fixed-width collapsed-buttons-row",
                  cls <-- tripExpanded.signal.map(expanded =>
                    if (expanded) "delayed-appear" else "",
                  ),
                  styleProp("opacity") <-- tripExpanded.signal.map(
                    expanded => if (expanded) "0" else "1",
                  ),
                  styleProp("pointer-events") <-- tripExpanded.signal
                    .map(expanded => if (expanded) "none" else "auto",
                    ),
                  span("+"),
                  onClick --> Observer { _ =>
                    tripExpanded.set(true)
                    isLocked.set(false) // Enter edit mode when adding trips
                  },
                ),

                // Expanded buttons container
                div(
                  cls := "expanded-buttons-row",
                  styleProp("pointer-events") <-- tripExpanded.signal
                    .map(expanded => if (expanded) "auto" else "none",
                    ),
                  styleProp("position") <-- tripExpanded.signal.map(
                    expanded =>
                      if (expanded) "relative" else "absolute",
                  ),

                  // New route button - emerges from center (left side)
                  button(
                    cls := "button button-fixed-width expand-from-left",
                    styleProp("transform") <-- tripExpanded.signal
                      .map(expanded =>
                        if (expanded) "translateX(0) scale(1)"
                        else "translateX(60px) scale(0)",
                      ),
                    styleProp("opacity") <-- tripExpanded.signal.map(
                      expanded => if (expanded) "1" else "0",
                    ),
                    "New trip",
                    onClick --> Observer { _ =>
                      addingNewRoute.set(true)
                      tripExpanded.set(false)
                    },
                  ),

                  // Return trip button - emerges from center (right side)
                  button(
                    cls := "button button-fixed-width expand-from-right",
                    styleProp("transform") <-- tripExpanded.signal
                      .map(expanded =>
                        if (expanded) "translateX(0) scale(1)"
                        else "translateX(-60px) scale(0)",
                      ),
                    styleProp("opacity") <-- tripExpanded.signal.map(
                      expanded => if (expanded) "1" else "0",
                    ),
                    "Return trip",
                    onClick --> Observer { _ =>
                      val plan = $plan.now()
                      val maybeLastSeg = plan.l.lastOption
                      maybeLastSeg.foreach { lastSeg =>
                        val options = returnTripEndpoints(lastSeg)
                        if options.hasAdjustments then
                          pendingReturnChoice.set(
                            Some(
                              evaluatePendingReturn(
                                lastSeg,
                                plan,
                                options,
                              ),
                            ),
                          )
                        else
                          attemptReturnTrip(
                            options.adjustedStart,
                            options.adjustedEnd,
                            Some(lastSeg.id),
                          )
                      }
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
      val suggestedName = $plan.now().routeSegments.headOption
        .map(seg => s"Trip from ${seg.start.l.name}")
        .getOrElse("New Trip")
      val enteredName = editingName.now().trim.take(20)
      val name = if (enteredName.nonEmpty) enteredName else suggestedName.take(20)
      val plan = $plan.now()
      val newSavedPlan = SavedPlan.create(plan, name)
      db.saveSavedPlan(newSavedPlan)
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
        db.saveSavedPlan(updatedPlan)
        $currentSavedPlan.set(Some(updatedPlan))
        originalPlanOnLoad.set(Some($plan.now()))
        isLocked.set(true)
        editingName.set("")
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
        .map { _ =>
          // Read current values - Vars have .now(), Signals need observe pattern
          val savedPlanO = $currentSavedPlan.now()
          val locked = isLocked.now()
          val hasPlans = hasSavedPlans.now()
          // For dirty, we check it directly since we have the signals
          val dirty = savedPlanO.exists { _ =>
            originalPlanOnLoad.now().exists(_ != $plan.now())
          }
          val isSaved = savedPlanO.isDefined
          
          Seq(
            // Edit times - only when locked
            BottomSheet.MenuItem(
              icon = "✏️",
              label = "Edit times",
              onClick = () => isLocked.set(false),
              hidden = !locked,
            ),
            // Save changes - only when dirty and saved
            BottomSheet.MenuItem(
              icon = "💾",
              label = "Save changes",
              onClick = () => {
                savedPlanO.foreach { sp =>
                  val updatedPlan = sp.withPlan($plan.now())
                  db.saveSavedPlan(updatedPlan)
                  $currentSavedPlan.set(Some(updatedPlan))
                  originalPlanOnLoad.set(Some($plan.now()))
                }
              },
              hidden = !dirty || !isSaved,
            ),
            // Save as new trip - for unsaved trips
            BottomSheet.MenuItem(
              icon = "💾",
              label = "Save trip",
              onClick = () => saveMode.set(true),
              hidden = isSaved,
            ),
            // Share
            BottomSheet.MenuItem(
              icon = "📤",
              label = "Share",
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
            ),
            // Notifications
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
              hidden = !NotificationCountdown.isSupported,
            ),
            // New trip
            BottomSheet.MenuItem(
              icon = "➕",
              label = "New trip",
              onClick = () => startNewTrip(),
            ),
            // Load trip
            BottomSheet.MenuItem(
              icon = "📂",
              label = "Load saved trip",
              onClick = () => {
                loadTripsMode.set(true)
                addingNewRoute.set(true)
              },
              hidden = !hasPlans,
            ),
            // Delete - only for saved trips
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
              hidden = !isSaved,
            ),
          )
      }

    div(
      // Only show when plan has segments
      display <-- $plan.signal.map(p => if (p.routeSegments.isEmpty) "none" else "block"),
      
      // Bottom sheet menu
      BottomSheet(menuOpen, "Trip Options", menuItems),
      
      // Main bar
      div(
        cls := "unified-bottom-bar",
        
        // Left side: Name trigger or edit input
        // Trigger on any state change, read values inside
        child <-- isLocked.signal
          .combineWith(saveMode.signal)
          .combineWith($currentSavedPlan.signal)
          .map { _ =>
            // Read current values directly to avoid type inference issues
            val locked = isLocked.now()
            val saving = saveMode.now()
            val savedPlanO = $currentSavedPlan.now()
            val isSaved = savedPlanO.isDefined
            
            if (saving) {
              // Save dialog for new trip
              val suggestedName = $plan.now().routeSegments.headOption
                .map(seg => s"Trip from ${seg.start.l.name}")
                .getOrElse("New Trip")
              div(
                cls := "bottom-bar-save-row",
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
              // View mode - show name with dropdown trigger
              div(
                cls := "bottom-bar-name-trigger",
                onClick --> Observer { _ => 
                  if (!isSaved) {
                    // Unsaved trip - go to save mode
                    saveMode.set(true)
                  } else {
                    menuOpen.set(true)
                  }
                },
                child <-- displayName.map { name =>
                  span(cls := "bottom-bar-name", name)
                },
                // Dirty indicator
                child <-- isDirtyNow.map { dirty =>
                  if (dirty) span(cls := "bottom-bar-dirty", "•")
                  else emptyNode
                },
                // Dropdown arrow (only for saved trips)
                if (isSaved)
                  span(cls := "bottom-bar-arrow", "▼")
                else
                  span(cls := "bottom-bar-hint", "tap to save"),
              )
            }
          },
      ),
    )
  }

  def RouteLegElement(
    routeSegment: RouteSegment,
    addingNewRoute: Var[Boolean],
    scheduleSelector: Observer[
      Option[SelectedStopInfo],
    ],
    legDeleter: Observer[RouteSegment],
    segmentUpdater: Observer[RouteSegment],
    segmentAppender: Observer[RouteSegment],
    $isLocked: Signal[Boolean] = Val(false),
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
        $isLocked = $isLocked,
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
        $isLocked,
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
        // Touch handlers for swipe-to-reveal (lock-aware via $isLocked signal)
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

  /** Plan name header with unified state machine:
    * 
    * States:
    * 1. UNSAVED - No saved plan, shows "Unsaved Trip" (clickable)
    * 2. SAVING - Shows inline save UI (input + Save + Cancel)
    * 3. SAVED_VIEWING - Locked saved plan, shows name + Edit button
    * 4. SAVED_EDITING - Unlocked saved plan, shows editable name input
    */
  def planNameAndLockRow(
    $plan: Var[Plan],
    $currentSavedPlan: Var[Option[SavedPlan]],
    isLocked: Var[Boolean],
    db: Persistence,
    focusPlanNameInput: Var[Boolean],
    hasSavedPlans: Var[Boolean],
    $isDirty: Signal[Boolean],
    originalPlanOnLoad: Var[Option[Plan]],
    saveExpanded: Var[Boolean],
  ) =
    // Local state for editing
    val editingName: Var[String] = Var("")

    // Derive the UI state from the signals
    sealed trait PlanNameState
    case object Unsaved extends PlanNameState
    case object Saving extends PlanNameState
    case class SavedViewing(plan: SavedPlan, isDirty: Boolean) extends PlanNameState
    case class SavedEditing(plan: SavedPlan) extends PlanNameState

    // Build state by reading current values in the map - simpler than combineWith
    val uiState: Signal[PlanNameState] = 
      $currentSavedPlan.signal
        .combineWith(isLocked.signal)
        .combineWith(saveExpanded.signal)
        .combineWith($isDirty)
        .map { _ =>
          // Read current values directly
          val savedPlanO = $currentSavedPlan.now()
          val locked = isLocked.now()
          val saving = saveExpanded.now()
          val dirty = savedPlanO.exists { sp =>
            originalPlanOnLoad.now().exists(_ != $plan.now())
          }
          
          (savedPlanO, locked, saving) match {
            case (None, _, true) => Saving
            case (None, _, _) => Unsaved
            case (Some(sp), true, _) => SavedViewing(sp, dirty)
            case (Some(sp), false, _) => SavedEditing(sp)
          }
        }

    // Helper to save a new plan
    def doSave(): Unit = {
      val suggestedName = $plan.now().routeSegments.headOption
        .map(seg => s"Trip from ${seg.start.l.name}")
        .getOrElse("New Trip")
      val enteredName = editingName.now().trim.take(20)
      val name = if (enteredName.nonEmpty) enteredName else suggestedName.take(20)
      val plan = $plan.now()
      val newSavedPlan = SavedPlan.create(plan, name)
      db.saveSavedPlan(newSavedPlan)
      $currentSavedPlan.set(Some(newSavedPlan))
      originalPlanOnLoad.set(Some(plan))
      hasSavedPlans.set(true)
      isLocked.set(true)
      editingName.set("")
      saveExpanded.set(false)
    }

    def cancelSave(): Unit = {
      editingName.set("")
      saveExpanded.set(false)
    }

    def discardChanges(sp: SavedPlan): Unit = {
      db.getSavedPlan(sp.id).foreach { freshPlan =>
        $plan.set(freshPlan.plan)
        originalPlanOnLoad.set(Some(freshPlan.plan))
        $currentSavedPlan.set(Some(freshPlan))
        isLocked.set(true)
      }
    }

    div(
      cls := "plan-name-row",
      child <-- uiState.map {
        case Unsaved =>
          // Clickable "Unsaved Trip" label
          span(
            cls := "plan-name-text plan-name-unsaved plan-name-clickable",
            "Unsaved Trip",
            title := "Click to save this trip",
            onClick.stopPropagation --> Observer { _ =>
              saveExpanded.set(true)
            },
          )

        case Saving =>
          // Inline save UI
          val suggestedName = $plan.now().routeSegments.headOption
            .map(seg => s"Trip from ${seg.start.l.name}")
            .getOrElse("New Trip")
          div(
            cls := "inline-save-row",
            input(
              cls := "plan-name-input",
              typ := "text",
              placeholder := suggestedName,
              maxLength := 20,
              onInput.mapToValue --> editingName.writer,
              onKeyDown --> Observer[dom.KeyboardEvent] { evt =>
                if (evt.key == "Enter") doSave()
                else if (evt.key == "Escape") cancelSave()
              },
              // Prevent parent touch handlers from interfering with cursor positioning
              TouchControls.onTouchStart.stopPropagation --> Observer.empty,
              TouchControls.onTouchMove.stopPropagation --> Observer.empty,
              onClick.stopPropagation --> Observer.empty,
            ),
            button(
              cls := "button inline-save-button",
              "Save",
              onClick.stopPropagation --> Observer { _ => doSave() },
            ),
            button(
              cls := "button button-outlined inline-cancel-button",
              "✕",
              title := "Cancel",
              onClick.stopPropagation --> Observer { _ => cancelSave() },
            ),
          )

        case SavedViewing(sp, isDirty) =>
          // Locked view: name + either Edit (clean) or Save/Discard (dirty)
          div(
            cls := "plan-name-display",
            span(cls := "plan-name-text", sp.displayName),
            if (isDirty) {
              // Dirty: show Save and Discard for route changes
              Seq(
                span(cls := "dirty-indicator", title := "Unsaved changes", "•"),
                button(
                  cls := "button edit-button",
                  "Save",
                  onClick --> Observer { _ =>
                    // Save current plan state to this SavedPlan
                    val updatedPlan = sp.withPlan($plan.now())
                    db.saveSavedPlan(updatedPlan)
                    $currentSavedPlan.set(Some(updatedPlan))
                    originalPlanOnLoad.set(Some($plan.now()))
                  },
                ),
                button(
                  cls := "button button-outlined discard-button",
                  "Discard",
                  title := "Discard unsaved changes",
                  onClick --> Observer { _ => discardChanges(sp) },
                ),
              )
            } else {
              // Clean: just show Edit for name changes
              Seq(
                button(
                  cls := "button edit-button",
                  "Edit",
                  onClick --> Observer { _ => isLocked.set(false) },
                ),
              )
            },
          )

        case SavedEditing(sp) =>
          // Unlocked: editable name input + Save/Discard
          div(
            cls := "plan-name-display",
            input(
              cls := "plan-name-input",
              typ := "text",
              maxLength := 30,
              placeholder := "Trip name",
              defaultValue := sp.name.getOrElse(""),
              onMountCallback { ctx =>
                editingName.set(sp.name.getOrElse(""))
              },
              onInput.mapToValue --> editingName.writer,
              // Prevent parent touch handlers from interfering with cursor positioning
              TouchControls.onTouchStart.stopPropagation --> Observer.empty,
              TouchControls.onTouchMove.stopPropagation --> Observer.empty,
              onClick.stopPropagation --> Observer.empty,
            ),
            button(
              cls := "button edit-button",
              "Save",
              onClick --> Observer { _ =>
                // Save both name and plan changes
                val finalName = editingName.now().trim
                val nameToUse = if (finalName.nonEmpty) finalName else sp.displayName
                val updatedPlan = sp.withName(nameToUse).withPlan($plan.now())
                db.saveSavedPlan(updatedPlan)
                $currentSavedPlan.set(Some(updatedPlan))
                originalPlanOnLoad.set(Some($plan.now()))
                isLocked.set(true)
              },
            ),
            button(
              cls := "button button-outlined discard-button",
              "Discard",
              onClick --> Observer { _ => discardChanges(sp) },
            ),
          )
      },
    )

  def copyButtons(
    $plan: Var[Plan],
    db: Persistence,
    isLocked: Var[Boolean],
    currentSavedPlan: Var[Option[SavedPlan]],
    addingNewRoute: Var[Boolean],
    loadTripsMode: Var[Boolean],
    focusPlanNameInput: Var[Boolean],
    hasSavedPlans: Var[Boolean],
    $isDirty: Signal[Boolean],
    originalPlanOnLoad: Var[Option[Plan]],
    saveExpanded: Var[Boolean],
  ) = {
    val shareExpanded: Var[Boolean] = Var(false)
    val tripName: Var[String] = Var("")
    val saveConfirmation: Var[Option[String]] = Var(None)

    // Add click handler to collapse when clicking outside
    val documentClickHandler: js.Function1[dom.MouseEvent, Unit] =
      (event: dom.MouseEvent) => {
        val target = event.target.asInstanceOf[dom.Element]
        val actionContainer =
          dom.document.querySelector(".share-save-buttons-container")
        if (
          actionContainer != null && !actionContainer.contains(target)
        ) {
          shareExpanded.set(false)
          saveExpanded.set(false)
          saveConfirmation.set(None)
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
      child <-- $plan.signal
        .combineWith(currentSavedPlan.signal)
        .map { case (plan, savedPlanO) =>
          if (plan.routeSegments.isEmpty)
            div()
          else {
            val isSaved = savedPlanO.isDefined

            div(
              cls := "centered",
              div(
                cls := "action-buttons-container share-save-buttons-container",

                // Collapsed state: Share, New, and Load buttons
                div(
                  cls := "expanded-buttons-row collapsed-buttons-row",
                  cls <-- shareExpanded.signal
                    .combineWith(saveExpanded.signal)
                    .map { case (share, save) =>
                      if (share || save) "delayed-appear" else ""
                    },
                  styleProp("opacity") <-- shareExpanded.signal
                    .combineWith(saveExpanded.signal)
                    .map { case (share, save) =>
                      if (share || save) "0" else "1"
                    },
                  styleProp(
                    "pointer-events",
                  ) <-- shareExpanded.signal
                    .combineWith(saveExpanded.signal)
                    .map { case (share, save) =>
                      if (share || save) "none" else "auto"
                    },
                  styleProp("position") <-- shareExpanded.signal
                    .combineWith(saveExpanded.signal)
                    .map { case (share, save) =>
                      if (share || save) "absolute" else "relative"
                    },
                  // Notification bell button
                  NotificationBellButton($plan),
                  button(
                    cls := "button button-compact",
                    SvgIcon.share("share-icon"),
                    onClick --> Observer { _ =>
                      shareExpanded.set(true)
                      saveExpanded.set(false)
                    },
                  ),
                  // New button - creates a fresh empty trip
                  button(
                    cls := "button button-compact",
                    "New",
                    onClick --> Observer { _ =>
                      // Create a fresh empty plan
                      val emptyPlan = Plan(Seq.empty)
                      // Save the empty plan as the current daily plan
                      db.saveDailyPlanOnly(emptyPlan)
                      // Update the plan
                      $plan.set(emptyPlan)
                      // Clear the current saved plan reference (doesn't delete the saved plan)
                      currentSavedPlan.set(None)
                      // Clear original plan tracking (no dirty state for new plans)
                      originalPlanOnLoad.set(None)
                      // Unlock the schedule so user can start adding routes
                      isLocked.set(false)
                      // Start in adding new route mode
                      addingNewRoute.set(true)
                    },
                  ),
                  // Load button - dynamically show/hide based on saved plans
                  child <-- hasSavedPlans.signal.map { hasPlans =>
                    if (hasPlans) {
                      button(
                        cls := "button button-compact",
                        "Load",
                        onClick --> Observer { _ =>
                          loadTripsMode.set(true)
                          addingNewRoute.set(true)
                        },
                      )
                    }
                    else {
                      emptyNode
                    }
                  },
                ),

                // Share expanded: Text and Link buttons
                div(
                  cls := "expanded-buttons-row",
                  styleProp(
                    "pointer-events",
                  ) <-- shareExpanded.signal
                    .map(expanded => if (expanded) "auto" else "none",
                    ),
                  styleProp("position") <-- shareExpanded.signal
                    .map(expanded =>
                      if (expanded) "relative" else "absolute",
                    ),
                  // Text button - emerges from Share position (left side, no translation needed)
                  button(
                    cls := "button button-fixed-width expand-from-left",
                    styleProp("transform") <-- shareExpanded.signal
                      .map(expanded =>
                        if (expanded) "translateX(0) scale(1)"
                        else "translateX(60px) scale(0)",
                      ),
                    styleProp("opacity") <-- shareExpanded.signal
                      .map(expanded => if (expanded) "1" else "0"),
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
                        dom.window.navigator.clipboard
                          .writeText(text)
                      }
                      shareExpanded.set(false)
                    },
                  ),
                  // Link button - emerges from Share position (needs to slide right to its final position)
                  button(
                    cls := "button button-fixed-width expand-from-right",
                    styleProp("transform") <-- shareExpanded.signal
                      .map(expanded =>
                        if (expanded) "translateX(0) scale(1)"
                        else "translateX(-120px) scale(0)",
                      ),
                    styleProp("opacity") <-- shareExpanded.signal
                      .map(expanded => if (expanded) "1" else "0"),
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
                            js.Dynamic.literal(
                              title = "Bus Schedule Link",
                              url = url,
                            ),
                          )
                      }
                      else {
                        dom.window.navigator.clipboard
                          .writeText(url)
                      }
                      shareExpanded.set(false)
                    },
                  ),
                ),

                // Save UI is now inline in planNameAndLockRow
              ),
            )
          }
        },
    )
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

    val routeSegmentsO =
      routesInPreferenceOrder.foldLeft(
        Option.empty[Seq[RouteSegment]],
      ) { case (acc, route) =>
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
  ) = {
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
              db.saveSavedPlan(newSavedPlan)
              Some(newSavedPlan)
            }
            else None
          }
          .flatten
      }
      // Purge all legacy plans now that migration is complete
      if (legacyNames.nonEmpty) {
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
                  cls := "button saved-trip-load-button",
                  "Load this trip",
                  onClick --> Observer { _ =>
                    // CRITICAL: Set currentSavedPlan BEFORE $plan to ensure observers
                    // use the correct plan ID if they fire during the transition
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
  ) =
    val startingPoint: Var[Option[Location]] = Var(None)
    val $locationsVar: Var[Seq[(Location, Int)]] = Var(Seq.empty)
    val $locations: Signal[Seq[(Location, Int)]] =
      $locationsVar.signal
    // Check if we should start in load trips mode
    val initialMode =
      if (loadTripsMode.now()) StopSelectorMode.LoadSavedTrip
      else StopSelectorMode.SelectStop
    val selectorMode: Var[StopSelectorMode] = Var(initialMode)

    def loadLocations(): Unit = {
      $locationsVar.set(Seq.empty)
      locations.zipWithIndex.foreach { l =>
        setTimeout(l._2 * 30) {
          $locationsVar.update(_ :+ l)
        }
      }
    }

    div(
      onMountCallback { ctx =>
        // Only load locations if we're in SelectStop mode (not LoadSavedTrip mode)
        // Don't reset loadTripsMode here - let it be controlled by user actions (Back button or loading a trip)
        if (initialMode == StopSelectorMode.SelectStop) {
          loadLocations()
        }
      },
      child <-- selectorMode.signal.map {
        case StopSelectorMode.LoadSavedTrip =>
          SavedTripsSelector(
            db,
            $plan,
            addingNewRoute,
            currentSavedPlan,
            isLocked,
            loadTripsMode,
            hasSavedPlans,
            originalPlanOnLoad,
          )
        case StopSelectorMode.SelectStop =>
          div(
            // Saved trips button - only show if there are saved trips
            Option
              .when(db.listPlanNames().nonEmpty)(
                button(
                  cls := "button button-outlined saved-trips-toggle m-2",
                  "Load saved trip",
                  onClick --> Observer { _ =>
                    selectorMode.set(StopSelectorMode.LoadSavedTrip)
                  },
                ),
              )
              .getOrElse(emptyNode),
            // Origin indicator - slides up when origin is selected
            div(
              cls := "origin-indicator-container",
              cls <-- startingPoint.signal.map {
                case Some(_) => "origin-indicator-visible"
                case None    => ""
              },
              child <-- startingPoint.signal.map {
                case Some(location) =>
                  div(
                    cls := "origin-indicator",
                    span(
                      cls := "origin-indicator-label",
                      "From:",
                    ),
                    span(
                      cls := "origin-indicator-name",
                      location.name,
                    ),
                    button(
                      cls := "origin-indicator-dismiss",
                      "✕",
                      onClick --> Observer { _ =>
                        startingPoint.set(None)
                      },
                    ),
                  )
                case None =>
                  emptyNode
              },
            ),
            h2(
              child.text <-- startingPoint.signal.map {
                case Some(_) => "Select your destination"
                case None    => "Select your origin"
              },
            ),
            div(
              children <-- $locations.splitTransition(identity) {
                case (_, (location, _), _, transition) =>
                  // Capture currentSavedPlan to prevent saving to wrong plan
                  val capturedSavedPlan = currentSavedPlan.now()
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
                                        // Don't auto-save - wait for explicit Save
                                        addingNewRoute.set(false)
                                        oldPlan.copy(l =
                                          oldPlan.l :+ matchingLeg,
                                        )
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
