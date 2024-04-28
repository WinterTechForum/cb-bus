package crestedbutte.laminar

import com.raquo.laminar.api.L.*
import crestedbutte.*
import crestedbutte.routes.{RtaNorthbound, RtaSouthbound}
import org.scalajs.dom
import crestedbutte.laminar.Experimental.getLocation
import crestedbutte.pwa.Persistence
import com.billding.time.{MinuteDuration, WallTime}
import com.raquo.laminar.nodes.ReactiveHtmlElement
import crestedbutte.NotificationStuff.desiredAlarms
import crestedbutte.*
import crestedbutte.dom.BulmaLocal
import crestedbutte.routes.{
  AllRoutes,
  RtaSouthbound,
  SpringFallLoop,
  TownShuttleTimes,
}
import org.scalajs.dom

import java.time.format.{DateTimeFormatter, FormatStyle}
import java.time.{Clock, Instant, OffsetDateTime}
import scala.concurrent.duration.FiniteDuration
import crestedbutte.dom.BulmaLocal.ModalMode
import crestedbutte.laminar.TouchControls.Swipe
import org.scalajs.dom.HTMLAnchorElement

import scala.collection.immutable.{AbstractSeq, LinearSeq}

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

  import org.scalajs.dom.window
  import crestedbutte.pwa.Persistence

  def PlanElement(
    plan: Plan,
    db: Persistence,
    $plan: Var[Plan],
    initialTime: WallTime,
    timestamp: WallTime,
  ) =
    val nextLegDirection: Var[Option[NamedRoute]] = Var(
      None,
    )
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

      val nextAfter = routeWithTimes.nextAfter(routeSegment)
      val nextBefore = routeWithTimes.nextBefore(routeSegment)

      val deleteButton: ReactiveHtmlElement[HTMLAnchorElement] =
        a(
          cls := "link",
          onClick --> Observer { _ =>
            val newPlan =
              plan.copy(l = plan.l.filterNot(_ == routeSegment))
            db.saveDailyPlanOnly(newPlan)
            $plan.set(newPlan)
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

      div(
        TouchControls.swipeProp {
          case Swipe.Left =>
            println("Swiping left and updating plan")
            nextAfter match
              case Some(nextAfterValue) =>
                val newPlan =
                  plan.copy(l =
                    plan.l.updated(planIndex, nextAfterValue),
                  )
                $plan.set(newPlan)
                db.saveDailyPlanOnly(newPlan)
              case None => ()
          case Swipe.Right =>
            nextBefore match {
              case Some(nextBeforeValue) =>
                val newPlan =
                  plan.copy(l =
                    plan.l.updated(planIndex, nextBeforeValue),
                  )
                $plan.set(newPlan)
                db.saveDailyPlanOnly(newPlan)
              case None => ()
            }
        },
        div(
          Seq(routeSegment.start, routeSegment.end).map(stop =>
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
                        .getUpcomingArrivalInfo(scheduleAtStop,
                                                stop.t,
                        )
                        .content match
                        case Left(stopTimeInfo: StopTimeInfo) =>
                          StopTimeInfoForLocation(
                            stopTimeInfo,
                            scheduleAtStop,
                          )
                        case Right(value) => div("-")

//                      scheduleAtStop.times.map(t => div(t.toDumbAmericanString))
                    }*,
                ),

              ),
            ),
          ) :+ div( // TODO Move this separator outside of this, so it's not attached to the last leg of the trip
            textAlign := "center",
            div("."),
            div("."),
          ),
        ),
      )

    div(
      plan.l.zipWithIndex.map { case (routeSegment, idx) =>
        div(
          RouteLegElement(
            routeSegment,
            idx,
            db,
          ),
        )
      }.foldLeft(div()){
        case (acc, next) => 
          acc.amend(next)
      },
      div(
        div(cls := "route-header mt-6", "Where to next?"),
        child <-- nextLegDirection.signal.map {
          case None =>
            div(
              cls := "route-header",
              button(
                cls := "button m-2",
                onClick --> Observer { _ =>
                  nextLegDirection.set(
                    Some(RtaSouthbound.fullSchedule),
                  )
                },
                "Crested Butte --> Gunnison",
              ),
              button(
                cls := "button m-2",
                onClick --> Observer { _ =>
                  nextLegDirection.set(
                    Some(RtaNorthbound.fullSchedule),
                  )
                },
                "Gunnison --> Crested Butte",
              ),
            )
          case Some(value) =>
            div(
              span(cls := "m-2 b h3",
                   "Route: " + value.componentName.userFriendlyName,
              ),
              button(
                cls := "button",
                "Switch Direction",
                onClick --> Observer { _ =>
                  nextLegDirection.update {
                    case Some(RtaNorthbound.fullSchedule) =>
                      Some(RtaSouthbound.fullSchedule)
                    case Some(RtaSouthbound.fullSchedule) =>
                      Some(RtaNorthbound.fullSchedule)
                    case _ => None
                  }
                },
              ),
              smallStopSelector(value,
                                $plan,
                                db,
                                initialTime,
                                nextLegDirection,
              ),
            )
        },
      ),
    )

  import com.raquo.laminar.api.L._

  def FullApp(
    pageMode: AppMode,
    initialComponent: Option[ComponentName],
    javaClock: Clock,
  ) = {
    val db = Persistence()

    val clockTicks = new EventBus[Unit]

    val components = AllRoutes.components(pageMode)

    val selectedComponent: Var[ComponentData] = Var(
      initialComponent
        .flatMap(initialRoute =>
          components.find: component =>
            component.componentName
              .elementNameMatches(initialRoute.name),
        )
        .getOrElse(
          PlanViewer,
//          RtaSouthbound.fullSchedule,
        ),
    )

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
        selectedComponent.now() != PlanViewer &&
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
//      Bulma.menu(selectedComponent, components),
      RepeatingElement()
        .repeatWithInterval( // This acts like a Dune thumper
          (),
          new FiniteDuration(5, scala.concurrent.duration.SECONDS),
        ) --> clockTicks,
      overallPageLayout(
        selectedComponent,
        timeStamps,
        pageMode,
        initialTime,
        db,
      ),
    )
  }

  def overallPageLayout(
    $selectedComponent: Var[ComponentData],
    timeStamps: Signal[WallTime],
    pageMode: AppMode,
    initialTime: WallTime,
    db: Persistence,
  ) = {
    println(Instant.now())
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

    val upcomingArrivalData =
      $selectedComponent.signal
        .combineWith(timeStamps)
        .map { case (componentData, timestamp) =>
          // This is a super janky way to avoid being unable to scroll
          // after we refresh the page and close the model
          org.scalajs.dom.document
            .querySelector("html")
            .classList
            .remove("is-clipped")
          componentData match {
            case PlanViewer =>
              TripViewerLaminar(
                db,
                initialTime,
                timestamp,
              )
            case _: NamedRoute =>
              throw new IllegalStateException(
                "Not supported anymore :D :D :D :D :D",
              )
          }
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
            Components.PlanElement(plan,
                                   db,
                                   $plan,
                                   initialTime,
                                   timestamp,
            ),
          ),
        ),
      ),
    )

  def smallStopSelector(
    namedRoute: NamedRoute,
    $plan: Var[Plan],
    db: Persistence,
    initialTime: WallTime,
    nextLegDirection: Var[Option[NamedRoute]], // TODO Smaller type
  ) =
    val startingPoint: Var[Option[Location]] = Var(None)
    val destinations: Signal[Option[Seq[Location]]] =
      startingPoint.signal.map {
        case Some(value) =>
          Some(
            namedRoute.routeWithTimes.allStops
              .dropWhile(_.location != value)
              .drop(1)
              .map(_.location),
          )
        case None => None
      }

    val allStops = namedRoute.routeWithTimes.allStops
    val startingPoints =
      div(
        "Select starting point: ",
        allStops.init.map(stop =>
          div(
            button(
              cls := "button m-2",
              stop.location.name,
              onClick.mapTo(Some(stop.location)) --> startingPoint,
            ),
          ),
        ),
      )
    div(
      child <-- startingPoint.signal.map {
        case Some(value) => "Starting at: " + value
        case None        => startingPoints
      },
      div(
        "Destination",
        child <-- destinations.signal.map {
          case Some(destinations) =>
            div(
              destinations.map(destination =>
                div(
                  button(
                    cls := "button m-2",
                    destination.name,
                    onClick --> Observer { _ =>
                      val start = startingPoint
                        .now()
                        .getOrElse(
                          throw Exception("No starting point"),
                        )

                      val matchingLeg =
                        namedRoute.routeWithTimes.legs
                          .flatMap { leg =>
                            leg.segmentFrom(start, destination)
                          }
                          .find { l =>
                            val lastArrivalTime =
                              $plan.now().l.lastOption
                                .map(_.end.t)
                            val cutoff =
                              lastArrivalTime.getOrElse(initialTime)
                            l.start.t.isAfter(cutoff)
                          }
                          .getOrElse(
                            // TODO Is the best fallback?
                            namedRoute.routeWithTimes.legs.last
                              .segmentFrom(start, destination)
                              .getOrElse(
                                throw new Exception(
                                  "Not route leg found with locations",
                                ),
                              ),
                          )

                      $plan.update { case oldPlan =>
                        val newPlan =
                          oldPlan.copy(l = oldPlan.l :+ matchingLeg)
                        db.saveDailyPlanOnly(newPlan)
                        nextLegDirection.set(None)
                        newPlan
                      }
                    },
                  ),
                ),
              ),
            )
          case None => div()
        },
      ),
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
        ),
      ),
    )
  }
}
