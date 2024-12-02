package crestedbutte.laminar

import com.billding.time.{MinuteDuration, WallTime}
import com.raquo.laminar.api.L.*
import crestedbutte.*
import crestedbutte.NotificationStuff.desiredAlarms
import crestedbutte.dom.BulmaLocal
import crestedbutte.laminar.Experimental.getLocation
import crestedbutte.laminar.TouchControls.Swipe
import crestedbutte.routes.{CompleteStopList, RtaNorthbound, RtaSouthbound}
import org.scalajs.dom
import org.scalajs.dom.HTMLAnchorElement

import java.time.format.DateTimeFormatter
import java.time.{Clock, Instant, OffsetDateTime}
import scala.concurrent.duration.FiniteDuration

enum SelectedSegmentPiece:
  case Start, End

case class LocationTimeDirection(
  locationWithTime: LocationWithTime,
  routeSegment: RouteSegment
                                )

object Components {
  def GeoBits(
               $mapLinksEnabled: Signal[Boolean],
               location: Location,
               $gpsPosition: Signal[Option[GpsCoordinates]],
             ) = {
    def distanceFromCurrentLocationToStop(
                                           gpsPosition: Signal[Option[GpsCoordinates]],
                                           location: Location,
                                         ) =
      gpsPosition.map(
        _.flatMap(userCords =>
          location.gpsCoordinates.map(stopCoords =>
            div(
              GpsCalculations
                .distanceInKmBetweenEarthCoordinatesT(
                  userCords,
                  stopCoords,
                ),
            ),
          ),
        ).getOrElse(div()),
      )

    div(
      child <-- $mapLinksEnabled.map(mapLinksEnabled =>
        if (mapLinksEnabled)
          div(
            cls := "map-link",
            child <--
              distanceFromCurrentLocationToStop($gpsPosition,
                location,
              ),
            location.gpsCoordinates.map(Components.GeoLink),
          )
        else
          div(),
      ),
    )
  }

  def GPS(
           gpsPosition: Var[Option[GpsCoordinates]],
         ) =
    button(
      idAttr := "Get position",
      onClick --> Observer[dom.MouseEvent]: ev =>
        getLocation(gpsPosition),
      "Get GPS coords",
    )

  def FeatureControlCenter(
                            featureUpdates: WriteBus[FeatureStatus],
                          ) = {

    // TODO Make this a separate component?
    def FeatureToggle(
                       feature: Feature,
                     ) =
      label(
        cls := "checkbox",
        feature.toString,
        input(
          typ := "checkbox",
          onInput.mapToChecked.map(
            FeatureStatus(feature, _),
          ) --> featureUpdates,
        ),
      )

    div(
      "Control Center",
      Feature.values.map(FeatureToggle),
    )
  }

  def GeoLink(
               gpsCoordinates: GpsCoordinates,
             ) =
    a(
      cls := "link",
      href := s"https://www.google.com/maps/search/?api=1&query=${gpsCoordinates.latitude},${gpsCoordinates.longitude}",
      SvgIcon("glyphicons-basic-592-map.svg"),
    )

  def SvgIcon(
               name: String,
               clsName: String = "",
             ) =
    img(
      cls := s"glyphicon $clsName",
      src := s"/glyphicons/svg/individual-svg/$name",
      alt := "Thanks for riding the bus!",
    )

  import com.raquo.laminar.nodes.ReactiveHtmlElement
  import crestedbutte.pwa.Persistence
  import org.scalajs.dom.window

  def PlanElement(
                   db: Persistence,
                   $plan: Var[Plan],
                   initialTime: WallTime,
                   timestamp: WallTime,
                   addingNewRoute: Var[Boolean],
                 ) = {
    // TODO Eventually pass this in as a param.
    val planSwipeUpdater: Observer[(Int, Option[RouteSegment])] =
      $plan.writer.contramap[(Int, Option[RouteSegment])]{ (idx, segmentO) =>
        segmentO match
          case Some(segment) => {
            val plan = $plan.now()
            val updatedPlan =
              plan.copy(l =
                plan.l.updated(idx, segment),
              )
            db.saveDailyPlanOnly(updatedPlan)
            updatedPlan
          }
          case None => $plan.now()
      }

    def RouteLegElement(
                         routeSegment: RouteSegment,
                         planIndex: Int,
                         db: Persistence,
                       ) =

      val routeWithTimes =
        routeSegment.route match
          case RtaSouthbound.componentName =>
            RtaSouthbound.fullSchedule.routeWithTimes
          case RtaNorthbound.componentName =>
            RtaNorthbound.fullSchedule.routeWithTimes
          case other =>
            throw new Exception("Unrecognized route: " + other)

      val deleteButton: ReactiveHtmlElement[HTMLAnchorElement] =
        a(
          cls := "link",
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
          /*
          Options:
            glyphicons-basic-193-circle-empty-remove.svg
            glyphicons-basic-373-times.svg
            glyphicons-basic-599-menu-close.svg
            glyphicons-basic-632-circle-minus.svg
            glyphicons-basic-639-octagon-remove-empty.svg
            glyphicons-basic-640-octagon-remove.svg
            glyphicons-basic-842-square-minus.svg
           */
          SvgIcon("glyphicons-basic-842-square-minus.svg",
            clsName = "delete",
          ),
        )

      def stopInfo(
                    routeSegment: RouteSegment,
                    $plan: Var[Plan],
                    selectedSegmentPiece: SelectedSegmentPiece
                  ) = {
        // TODO Update stopBeingImplicitlyChanged after explicit bit is calculated
        val (stopBeingExplicitlyChanged, stopBeingImplicitlyChanged) = selectedSegmentPiece match
          case SelectedSegmentPiece.Start => (routeSegment.start, routeSegment.end)
          case SelectedSegmentPiece.End => (routeSegment.end, routeSegment.start)
        val stop = stopBeingExplicitlyChanged
        UpcomingStopInfo(
          stop.l,
          if (
            stop == routeSegment.start
          ) // Only show delete beside start location
            deleteButton
          else
            div(),
          div(
            div(
              routeWithTimes.allStops
                .filter(_.location == stop.l)
                .map { scheduleAtStop =>
                  TimeCalculations
                    .getUpcomingArrivalInfo(stop.t, scheduleAtStop, timestamp)
                    .content match
                    case Left(stopTimeInfo: StopTimeInfo) =>
                      StopTimeInfoForLocation(
                        stopTimeInfo,
                        scheduleAtStop,
                        $plan.now(),
                        $plan.writer.contramap[LocationTimeDirection] { ltd =>
                          TimeCalculations.updateSegmentFromArbitrarySelection(ltd, $plan.now())
                        }
                      )
                    case Right(value) => div("-")
                } *,
            ),
          ),
        )
      }

      div(
        TouchControls.swipeProp {
          case Swipe.Left =>
            planSwipeUpdater.onNext(planIndex, routeWithTimes.nextAfter(routeSegment))
          case Swipe.Right =>
            planSwipeUpdater.onNext(planIndex, routeWithTimes.nextBefore(routeSegment))
        },
        div(
          cls := "plan-segments",
          // TODO pass state piece is being updated
          stopInfo(routeSegment, $plan, SelectedSegmentPiece.Start),

          /*
             Connecting icons for start and end of legs
            glyphicons-basic-211-arrow-down.svg
            glyphicons-basic-221-chevron-down.svg
            glyphicons-basic-796-set-down.svg
            glyphicons-basic-827-arrow-thin-down.svg
           */
          SvgIcon("glyphicons-basic-211-arrow-down.svg",
            "plain-white plan-segment-divider",
          ),
          stopInfo(routeSegment, $plan, SelectedSegmentPiece.End),
          div( // TODO Move this separator outside of this, so it's not attached to the last leg of the trip
            textAlign := "center",
            SvgIcon("glyphicons-basic-947-circle-more.svg",
              "plain-white plan-segment-divider",
            ),
            // TODO Possibly use this icon as a separator: glyphicons-basic-947-circle-more.svg
          ),
        ),
      )

    div(
      $plan.now().l.zipWithIndex
        .map { case (routeSegment, idx) =>
          div(
            RouteLegElement(
              routeSegment,
              idx,
              db,
            ),
          )
        }
        .foldLeft(div()) { case (acc, next) =>
          acc.amend(next)
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
              smallStopSelectorNew(
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
}

  import com.raquo.laminar.api.L.*

  def FullApp(
    pageMode: AppMode,
    javaClock: Clock,
  ) = {
    val db = Persistence()

    val clockTicks = new EventBus[Unit]

    val selectedComponent = PlanViewer

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
      .filter(_ =>
        selectedComponent != PlanViewer &&
          // Don't reset content if we're in the middle of a modal
          !org.scalajs.dom.document
            .querySelector("html")
            .classList
            .contains("is-clipped"),
      )
      .foldLeft(
        initialTime,
      )(
        (
          _,
          _,
        ) => currentWallTime(javaClock),
      )

    div(
      onMountCallback: context =>
        db.initializeOrResetStorage(),
      RepeatingElement()
        .repeatWithInterval( // This acts like a Dune thumper
          (),
          new FiniteDuration(5, scala.concurrent.duration.SECONDS),
        ) --> clockTicks,
      overallPageLayout(
        timeStamps,
        pageMode,
        initialTime,
        db,
      ),
    )
  }

  def overallPageLayout(
    timeStamps: Signal[WallTime],
    pageMode: AppMode,
    initialTime: WallTime,
    db: Persistence,
  ) = {
    // TODO Turn this into a Signal. The EventBus should be contained within the Experimental/FeatureControlCenter
    val featureUpdates = new EventBus[FeatureStatus]

    val initialFeatureSets = FeatureSets(
      Feature.values.map((_, false)).toMap,
    )

    val $enabledFeatures: Signal[FeatureSets] =
      featureUpdates.events
        .foldLeft[FeatureSets](initialFeatureSets) {
          case (currentFeatures, featureUpdate) =>
            currentFeatures.update(featureUpdate)
        }

    val gpsPosition: Var[Option[GpsCoordinates]] = Var(None)

    val upcomingArrivalData = timeStamps
      .map { timestamp =>
        // This is a super janky way to avoid being unable to scroll
        // after we refresh the page and close the model
        org.scalajs.dom.document
          .querySelector("html")
          .classList
          .remove("is-clipped")
        TripViewerLaminar(
          db,
          initialTime,
          timestamp,
        )
      }

    div(
      div(
        cls := "bill-box",
        idAttr := "container",
        child <-- upcomingArrivalData, // **THIS IS THE IMPORTANT STUFF** The fact that it's hard to see means I need to remove other bullshit
        timeStamps --> Observer[WallTime](
          onNext = localTime =>
            desiredAlarms
              .dequeueAll(busTime =>
                localTime
                  .between(busTime)
                  // TODO Direct comparison
                  .toMinutes <= NotificationStuff.headsUpAmount.toMinutes,
              )
              .map(
                Experimental.Notifications
                  .createJankyBusAlertInSideEffectyWay(_, localTime),
              ),
        ),
        Option.when(pageMode == AppMode.dev)(
          Experimental.Sandbox(
            timeStamps,
            gpsPosition,
            featureUpdates: EventBus[FeatureStatus],
          ),
        ),
      ),
    )
  }

  object UpcomingStopInfo {
    def apply(
      location: Location,
      deleteButton: ReactiveHtmlElement[_],
      content: ReactiveHtmlElement[_],
    ) =
      div(
        width := "100%",
        cls := "stop-information",
        div(cls := "stop-name", div(location.name)),
        div(cls := "stop-alt-name", div(location.altName)),
        div(cls := "upcoming-information", content),
        div(cls := "map-link", deleteButton),
      )
  }

  def TripViewerLaminar(
    db: Persistence,
    initialTime: WallTime,
    timestamp: WallTime,
  ) =

    val $plan: Var[Plan] = Var(
      db.retrieveDailyPlanOnly.getOrElse(Plan(Seq.empty)),
    )
    val addingNewRoute: Var[Boolean] = Var(
      true,
    )
    div(
      child <-- $plan.signal.map(plan =>
        div(
          div(
            if (plan.l.isEmpty)
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
                    val url =
                      if (dom.document.URL.contains("localhost"))
                        s"http://localhost:8000/index_dev.html?plan=${UrlEncoding.encode(plan)}"
                      else
                        s"https://cbbus.netlify.app/?plan=${UrlEncoding.encode(plan)}"

                    dom.window.navigator.clipboard
                      .writeText(url)
                  },
                ),
              ),
            Components.PlanElement(
                                   db,
                                   $plan,
                                   initialTime,
                                   timestamp,
                                   addingNewRoute,
            ),
          ),
        ),
      ),
    )

  def rightLegOnRightRoute(
    start: Location,
    end: Location,
    plan: Plan,
    pageLoadTime: WallTime,
  ): RouteSegment = {

    val routeSegments =
      RtaSouthbound.fullSchedule
        .segment(start, end)
        .orElse(RtaNorthbound.fullSchedule.segment(start, end))

    routeSegments match
      case Some(segments) =>
        segments
          .find { l =>
            val lastArrivalTime =
              plan.l.lastOption
                .map(_.end.t)
            val cutoff =
              lastArrivalTime.getOrElse(pageLoadTime)
            println(s"Looking for start time after $cutoff")
            l.start.t.isLikelyEarlyMorningRatherThanLateNight || l.start.t
              .isAfter(cutoff)
          }
          .getOrElse {
            println("A")
            // TODO Confirm I can delete this possibility?
            throw new IllegalStateException(
              "No route leg available in either route. Start: " + start + "  End: " + end,
            )
          }
      case None =>
        println("B")
        throw new IllegalStateException(
          "No route leg available in either route B",
        )

  }

  def smallStopSelectorNew(
    locations: Seq[Location],
    $plan: Var[Plan],
    db: Persistence,
    initialTime: WallTime,
    addingNewRoute: Var[Boolean], // TODO Smaller type
  ) =
    val startingPoint: Var[Option[Location]] = Var(None)

    // Do a more unified view in the start/stop selection.
    // Shouldn't be 2 completely separate groups of elements
    val startingPoints =
      div(
        div("Starting at: "),
        div("____________________"),
        div(
          color := "grey",
          span("|__________________|"),
        ), // Really just to keep vertical spacing from changing after you select a start point
        locations.map(location =>
          div(
            button(
              cls := "button m-2",
              onClick --> Observer { _ =>
                println("In observer: " + location)
                startingPoint.update {
                  case Some(startingPointNow)
                      if startingPointNow == location =>
                    println("Clearing out startingPoint")
                    None
                  case Some(other) =>
                    try {
                      println("Trying to connect dots")
                      val matchingLeg =
                        rightLegOnRightRoute(
                          other,
                          location,
                          $plan.now(),
                          initialTime,
                        )
                      println("Connected dots")

                      $plan.update { case oldPlan =>
                        val newPlan =
                          oldPlan.copy(l = oldPlan.l :+ matchingLeg)
                        db.saveDailyPlanOnly(newPlan)
                        println(
                          "setting addingNewRoute to false",
                        )
                        addingNewRoute.set(false)
                        println(
                          "Adding new route: " + addingNewRoute
                            .now(),
                        )
                        newPlan
                      }
                      Some(other)
                    }
                    catch {
                      case ex: Throwable =>
                        println("Illegal state: " + ex)
                        throw new IllegalStateException(ex.getMessage)
                    }
                  case None =>
                    Some(location)
                }
              },
              cls <-- startingPoint.signal.map { sp =>
                sp match
                  case Some(startingPointNow)
                      if startingPointNow == location =>
                    println("startingPointNow: " + startingPointNow)
                    "is-primary"
                  case Some(_) => ""
                  case None    => ""
              },
              location.name,
            ),
          ),
        ),
      )
    div(
      startingPoints,
    )

  def renderWaitTime(
    duration: MinuteDuration,
  ) =
    if (duration.toMinutes == 0)
      "Leaving!"
    else
      duration.toMinutes + " min."

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
    stopTimeInfo: StopTimeInfo,
    busScheduleAtStop: BusScheduleAtStop,
    plan: Plan, // TODO Get rid of this
    selectedTimeUpdater: Sink[LocationTimeDirection]
    // TODO pass state piece is being updated
  ) = {

    val modalActive = Var(false)
    div(
      button(
        cls := "arrival-time button open-arrival-time-modal",
        onClick.preventDefault.map { _ =>
          org.scalajs.dom.document
            .querySelector("html")
            .classList
            .add("is-clipped")
          true
        } --> modalActive,
        stopTimeInfo.time.toDumbAmericanString,
      ),
      div(
        cls := "wait-time",
        renderWaitTime(stopTimeInfo.waitingDuration),
        BulmaLocal.bulmaModal(
          busScheduleAtStop,
          modalActive,
          plan,
          selectedTimeUpdater
        ),
      ),
    )
  }
}
