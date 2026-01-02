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

case class SavedTrip(
  name: String,
  plan: Plan)

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
      copyButtons($plan.signal, db),
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

            case class PendingReturnTrip(
              lastSegmentId: Long,
              options: ReturnTripOptions,
              adjustedAvailable: Boolean,
              originalAvailable: Boolean,
              generalMessage: Option[String] = None,
            )

            val pendingReturnChoice
              : Var[Option[PendingReturnTrip]] =
              Var(None)

            def evaluatePendingReturn(
              lastSegment: RouteSegment,
              plan: Plan,
              options: ReturnTripOptions,
            ): PendingReturnTrip =
              val lastEndTime = lastSegment.end.t
              def hasReturnLeg(start: Location, end: Location) =
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
                      db.saveDailyPlanOnly(updatedPlan)
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
                              generalMessage = Some(unavailableMessage),
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
                        val options = returnTripEndpoints(lastSeg)
                        if (options.hasAdjustments) then
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

                  // Notification toggle button
                  // TODO Re-enable if I can ever get it working reliably
                  // NotificationToggleButton(
                  //   $plan,
                  //   timeStamps,
                  //   buttonWidth,
                  // ),
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
    db: Persistence,
  ) = {
    val shareExpanded: Var[Boolean] = Var(false)
    val saveExpanded: Var[Boolean] = Var(false)
    val tripName: Var[String] = Var("")
    val saveConfirmation: Var[Option[String]] = Var(None)

    // Add click handler to collapse when clicking outside
    val documentClickHandler: js.Function1[dom.MouseEvent, Unit] =
      (event: dom.MouseEvent) => {
        val target = event.target.asInstanceOf[dom.Element]
        val actionContainer =
          dom.document.querySelector(".action-buttons-container")
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
      child <-- $plan.map { plan =>
        if (plan.routeSegments.isEmpty)
          div()
        else {
          val anyExpanded = shareExpanded.signal.combineWith(saveExpanded.signal).map {
            case (share, save) => share || save
          }

          div(
            cls := "centered",
            div(
              cls := "action-buttons-container trip-button-container",

              // Collapsed state: Share and Save buttons side by side
              div(
                cls := "controls-row",
                styleProp("opacity") <-- anyExpanded.map(
                  expanded => if (expanded) "0" else "1",
                ),
                styleProp("pointer-events") <-- anyExpanded.map(
                  expanded => if (expanded) "none" else "auto",
                ),
                styleProp("position") := "absolute",
                button(
                  cls := "button button-fixed-width",
                  "Share",
                  onClick --> Observer { _ =>
                    shareExpanded.set(true)
                    saveExpanded.set(false)
                  },
                ),
                button(
                  cls := "button button-fixed-width",
                  "Save",
                  onClick --> Observer { _ =>
                    saveExpanded.set(true)
                    shareExpanded.set(false)
                    saveConfirmation.set(None)
                  },
                ),
              ),

              // Share expanded: Text and Link buttons
              div(
                cls := "expanded-buttons-row",
                styleProp("opacity") <-- shareExpanded.signal.map(
                  expanded => if (expanded) "1" else "0",
                ),
                styleProp("pointer-events") <-- shareExpanded.signal
                  .map(expanded =>
                    if (expanded) "auto" else "none",
                  ),
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
                    setTimeout(300)(shareExpanded.set(false))
                  },
                ),
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
                    setTimeout(300)(shareExpanded.set(false))
                  },
                ),
              ),

              // Save expanded: input and Save Trip button
              div(
                cls := "expanded-buttons-row",
                styleProp("opacity") <-- saveExpanded.signal.map(
                  expanded => if (expanded) "1" else "0",
                ),
                styleProp("pointer-events") <-- saveExpanded.signal
                  .map(expanded =>
                    if (expanded) "auto" else "none",
                  ),
                child <-- saveConfirmation.signal.map {
                  case Some(name) =>
                    div(
                      cls := "save-confirmation",
                      span(s"Saved as '$name'"),
                    )
                  case None =>
                    div(
                      cls := "expanded-buttons-row",
                      input(
                        cls := "save-input",
                        typ := "text",
                        placeholder := "Trip name",
                        controlled(
                          value <-- tripName.signal,
                          onInput.mapToValue --> tripName.writer,
                        ),
                      ),
                      button(
                        cls := "button button-fixed-width",
                        "Save Trip",
                        disabled <-- tripName.signal.map(
                          _.trim.isEmpty,
                        ),
                        onClick --> Observer { _ =>
                          val name = tripName.now().trim
                          if (name.nonEmpty) {
                            db.savePlanByName(name, plan)
                            saveConfirmation.set(Some(name))
                            tripName.set("")
                            setTimeout(1500) {
                              saveExpanded.set(false)
                              saveConfirmation.set(None)
                            }
                          }
                        },
                      ),
                    )
                },
              ),
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
    adjustedEnd: Location,
  ) {
    val startAdjusted = originalStart != adjustedStart
    val endAdjusted = originalEnd != adjustedEnd
    val hasAdjustments = startAdjusted || endAdjusted
  }

  /** Apply the rec-center/spencer special-casing requested for return trips
    * while keeping the opposite stop unchanged, but also keep the original
    * endpoints so we can prompt the user when we make a change.
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

  sealed trait StopSelectorMode
  object StopSelectorMode {
    case object SelectStop extends StopSelectorMode
    case object LoadSavedTrip extends StopSelectorMode
  }

  def SavedTripsSelector(
    db: Persistence,
    $plan: Var[Plan],
    addingNewRoute: Var[Boolean],
    onBack: () => Unit,
  ) = {
    val $savedTripsVar: Var[Seq[(String, Int)]] = Var(Seq.empty)

    def loadSavedTrips(): Unit = {
      $savedTripsVar.set(Seq.empty)
      val names = db.listPlanNames()
      names.zipWithIndex.foreach { case (name, idx) =>
        setTimeout(idx * 30) {
          $savedTripsVar.update(_ :+ (name, idx))
        }
      }
    }

    div(
      cls := "saved-trips-selector",
      onMountCallback { _ =>
        loadSavedTrips()
      },
      button(
        cls := "button button-outlined m-2",
        "← Back to stops",
        onClick --> Observer { _ =>
          onBack()
        },
      ),
      h2("Load a saved trip"),
      child <-- $savedTripsVar.signal.map { trips =>
        if (trips.isEmpty && db.listPlanNames().isEmpty)
          div(
            cls := "no-saved-trips",
            p("No saved trips yet."),
            p("Create a trip and save it to see it here."),
          )
        else
          emptyNode
      },
      div(
        children <-- $savedTripsVar.signal.splitTransition(identity) {
          case (_, (name, _), _, transition) =>
            val tripPlanO = db.getPlanByName(name)
            div(
              transition.height,
              cls := "saved-trip-card",
              div(
                cls := "saved-trip-card-header",
                span(cls := "saved-trip-name", name),
                button(
                  cls := "saved-trip-delete",
                  "✕",
                  onClick --> Observer { _ =>
                    db.deletePlanByName(name)
                    $savedTripsVar.update(_.filterNot(_._1 == name))
                  },
                ),
              ),
              tripPlanO match {
                case Some(plan) =>
                  div(
                    cls := "saved-trip-segments",
                    plan.routeSegments.map { segment =>
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
                  )
                case None =>
                  div(cls := "saved-trip-error", "Could not load trip")
              },
              button(
                cls := "button saved-trip-load-button",
                "Load this trip",
                onClick --> Observer { _ =>
                  tripPlanO.foreach { plan =>
                    $plan.set(plan)
                    db.saveDailyPlanOnly(plan)
                    addingNewRoute.set(false)
                  }
                },
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
  ) =
    val startingPoint: Var[Option[Location]] = Var(None)
    val $locationsVar: Var[Seq[(Location, Int)]] = Var(Seq.empty)
    val $locations: Signal[Seq[(Location, Int)]] =
      $locationsVar.signal
    val selectorMode: Var[StopSelectorMode] =
      Var(StopSelectorMode.SelectStop)

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
        loadLocations()
      },
      child <-- selectorMode.signal.map {
        case StopSelectorMode.LoadSavedTrip =>
          SavedTripsSelector(
            db,
            $plan,
            addingNewRoute,
            onBack = () => {
              selectorMode.set(StopSelectorMode.SelectStop)
              loadLocations()
            },
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

  /** Voting state for a topic */
  sealed trait VoteState
  object VoteState {
    case object None extends VoteState
    case object Interested extends VoteState
    case object NotInterested extends VoteState
  }

  /** Voting buttons component for topics.
    * Displays a heart icon for "interested" and an X icon for "not interested".
    * When a button is selected, it appears highlighted/pressed.
    *
    * @param topicId Unique identifier for the topic being voted on
    * @param voteState A Var holding the current vote state
    * @param onVoteChange Optional callback when vote changes
    */
  def VotingButtons(
    topicId: String,
    voteState: Var[VoteState],
    onVoteChange: Option[Observer[VoteState]] = scala.None,
  ): ReactiveHtmlElement[HTMLDivElement] = {
    def handleVote(newState: VoteState): Unit = {
      val currentState = voteState.now()
      // Toggle off if clicking the same button, otherwise set new state
      val nextState =
        if (currentState == newState) VoteState.None else newState
      voteState.set(nextState)
      onVoteChange.foreach(_.onNext(nextState))
    }

    div(
      cls := "voting-buttons",
      dataAttr("topic-id") := topicId,
      // Interested (heart) button
      button(
        cls := "voting-button voting-button-interested",
        cls <-- voteState.signal.map {
          case VoteState.Interested => "voting-button-selected"
          case _                    => ""
        },
        onClick --> Observer { _ => handleVote(VoteState.Interested) },
        SvgIcon.heart("voting-icon"),
        span(cls := "voting-label", "Interested"),
      ),
      // Not Interested (times/X) button
      button(
        cls := "voting-button voting-button-not-interested",
        cls <-- voteState.signal.map {
          case VoteState.NotInterested => "voting-button-selected"
          case _                       => ""
        },
        onClick --> Observer { _ =>
          handleVote(VoteState.NotInterested)
        },
        SvgIcon.times("voting-icon"),
        span(cls := "voting-label", "Not interested"),
      ),
    )
  }
}
