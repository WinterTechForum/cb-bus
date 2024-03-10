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
import java.time.{Clock, OffsetDateTime}
import scala.concurrent.duration.FiniteDuration
import crestedbutte.dom.BulmaLocal.ModalMode

import scala.collection.immutable.{AbstractSeq, LinearSeq}

object Components {
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
  ) =
    img(
      cls := "glyphicon",
      src := s"/glyphicons/svg/individual-svg/$name",
      alt := "Thanks for riding the bus!",
    )

  def RouteLegElementInteractive(
    routeLeg: RouteLeg,
    db: Persistence,
    $active: Var[Boolean],
    $notifications: Observer[ReactiveHtmlElement[_]],
    componentSelector: Observer[ComponentData],
  ) =
    val clickBus = EventBus[RouteLeg]()
    div(
      routeLeg.stops.head match

        case LocationWithTime(
              location,
              busTime,
            ) =>
          UpcomingStopInfo(
            location,
            div(
              busTime.toDumbAmericanString,
            ),
          )
      ,
      div(
        cls := "scrollable-route-leg",
        clickBus.events.map(routeLeg =>
          BulmaLocal.notificationWithHomeLink("Trip Plan Updated.",
                                              componentSelector,
          ),
        ) --> $notifications,
        clickBus.events
          .map { (e: RouteLeg) =>
            println("routeLeg before the explosion: " + e)
            db.updateDailyPlan(e)
            true // keep modal open
          } --> $active,
        routeLeg.stops.tail.map(stop =>
          UpcomingStopInfo(
            stop.location,
            div(
              span(
                stop.busTime.toDumbAmericanString,
              ),
              button(
                cls := "button",
                onClick.preventDefault
                  .mapTo {
                    val head =
                      routeLeg.stops.headOption.getOrElse(
                        throw new IllegalStateException(
                          "Can't add a leg with a missing head.",
                        ),
                      )

                    RouteLeg(Seq(head, stop), routeLeg.routeName)
                      .getOrElse(
                        throw new IllegalStateException(
                          "Failed to create new route leg",
                        ),
                      )
                  } --> clickBus,
                "+",
              ),
            ),
          ),
        ),
      ),
    )

  import com.raquo.laminar.nodes.ReactiveHtmlElement

  import org.scalajs.dom.window
  import crestedbutte.pwa.Persistence

  def PlanElement(
    plan: Plan,
    db: Persistence,
    $plan: Var[Plan],
    initialTime: WallTime,
  ) =
    val nextLegDirection: Var[NamedRoute] = Var(
      RtaNorthbound.fullSchedule,
    )
    def RouteLegElementViewOnly(
      label: String,
      routeLeg: RouteLeg,
      planIndex: Int,
      db: Persistence,
    ) =

      val routeWithTimesO =
        routeLeg.routeName match
          case RtaSouthbound.componentName =>
            Some(RtaSouthbound.fullSchedule.routeWithTimes)
          case RtaNorthbound.componentName =>
            Some(RtaNorthbound.fullSchedule.routeWithTimes)
          case _ => None
      val nextAfter =
        for
          routeWithTimes <- routeWithTimesO
          nextAfter      <- routeWithTimes.nextAfter(routeLeg)
        yield nextAfter
      val nextBefore =
        for
          routeWithTimes <- routeWithTimesO
          nextAfter      <- routeWithTimes.nextBefore(routeLeg)
        yield nextAfter

      div(
        div(label),
        div(
          // TODO Make a way to delete leg of a trip here
          nextBefore match
            case Some(nextBeforeValue) =>
              button(
                cls := "button",
                "<",
                onClick --> Observer { _ =>
                  routeWithTimesO match
                    case Some(value) =>
                      val newPlan =
                        plan.copy(legs =
                          plan.legs.updated(planIndex,
                                            nextBeforeValue,
                          ),
                        )
                      $plan.set(newPlan)
                      db.saveDailyPlanOnly(newPlan)
                    case None =>
                      throw new Exception("no routeWithTimesO")
                },
              )
            case None => span()
          ,
          button(
            cls := "button",
            "Delete",
            onClick --> Observer { _ =>
              val newPlan =
                plan.copy(legs = plan.legs.filterNot(_ == routeLeg))
              db.saveDailyPlanOnly(newPlan)
              $plan.set(newPlan)
            },
          ),
          nextAfter match
            case Some(nextAfterValue) =>
              button(
                cls := "button",
                ">",
                onClick --> Observer { _ =>
                  routeWithTimesO match
                    case Some(value) =>
                      val newPlan =
                        plan.copy(legs =
                          plan.legs.updated(planIndex,
                                            nextAfterValue,
                          ),
                        )
                      $plan.set(newPlan)
                      db.saveDailyPlanOnly(newPlan)
                    case None =>
                      throw new Exception("no routeWithTimesO... 2")
                },
              )
            case None => span()
          ,
          routeLeg.stops.map(stop =>
            UpcomingStopInfo(
              stop.location,
              div(
                stop.busTime.toDumbAmericanString,
              ),
            ),
          ),
        ),
      )

    div(
      button(
        cls := "button",
        "Copy to Clipboard",
        onClick --> Observer { _ =>
          dom.window.navigator.clipboard
            .writeText(plan.plainTextRepresentation)
        },
      ),
      plan.legs.zipWithIndex.map { case (routeLeg, idx) =>
        div(
          RouteLegElementViewOnly(
            "Trip " + (idx + 1),
            routeLeg,
            idx,
            db,
          ),
        )
      },
      div(
        child <-- nextLegDirection.signal.map(s =>
          span(s.componentName.userFriendlyName),
        ),
        button(
          cls := "button",
          "Switch Direction",
          onClick --> Observer { _ =>
            nextLegDirection.update {
              case RtaNorthbound.fullSchedule =>
                RtaSouthbound.fullSchedule
              case RtaSouthbound.fullSchedule =>
                RtaNorthbound.fullSchedule
            }
          },
        ),
        child <-- nextLegDirection.signal.map { direction =>
          smallStopSelector(direction, $plan, db, initialTime)
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
            println("Component: " + component.componentName.name)
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
      .roundToNextTenMutable()

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
      Bulma.menu(selectedComponent, components),
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
                $selectedComponent.writer,
                initialTime,
              )
            case namedRoute: NamedRoute =>
              TopLevelRoute(
                TimeCalculations
                  .getUpComingArrivalsWithFullScheduleNonZio(
                    timestamp,
                    namedRoute,
                  ),
                $enabledFeatures,
                gpsPosition,
                db,
                $selectedComponent.writer,
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
      content: ReactiveHtmlElement[_],
      $mapLinksEnabled: Signal[Boolean] = Signal.fromValue(false),
      // TODO Should this be an `Option[Signal[GpsCoordinates]` instead?
      $gpsPosition: Signal[
        Option[GpsCoordinates],
      ] = Signal.fromValue(None),
      /* TODO: waitDuration: Duration*/
    ) =
      div(
        width := "100%",
        cls := "stop-information",
        GeoBits($mapLinksEnabled, location, $gpsPosition),
        div(cls := "stop-name", div(location.name)),
        div(cls := "stop-alt-name", div(location.altName)),
        div(cls := "upcoming-information", content),
      )

    private def GeoBits(
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

  }

  object TopLevelRoute {

    def apply(
      upcomingArrivalComponentData: UpcomingArrivalComponentData,
      $enabledFeatures: Signal[FeatureSets],
      gpsPosition: Var[Option[GpsCoordinates]],
      db: Persistence,
      componentSelector: Observer[ComponentData],
    ) =

      def RouteHeader(
        routeName: ComponentName,
      ) =
        div(
          cls := "route-header",
          span(
            cls := "route-header_name",
            routeName.userFriendlyName + " Departures",
          ),
          SvgIcon("glyphicons-basic-32-bus.svg"),
        )

      div(
        RouteHeader(upcomingArrivalComponentData.routeName),
        upcomingArrivalComponentData.upcomingArrivalInfoForAllRoutes
          .map {
            case UpcomingArrivalInfoWithFullSchedule(
                  UpcomingArrivalInfo(_, content),
                  fullScheduleAtStop,
                  namedRoute,
                  location,
                ) =>
              UpcomingStopInfo(
                location,
                content match {
                  case Left(stopTimeInfo) =>
                    StopTimeInfoForLocation(
                      stopTimeInfo,
                      fullScheduleAtStop,
                      $enabledFeatures,
                      namedRoute,
                      db,
                      componentSelector,
                    )
                  case Right(safeRideRecommendation) =>
                    SafeRideLink(safeRideRecommendation)
                },
                $enabledFeatures.map(
                  //                _.isEnabled(Feature.MapLinks),
                  _ => true, // Maps always enabled now
                ),
                gpsPosition.signal,
              )
          },
      )

    def StopTimeInfoForLocation(
      stopTimeInfo: StopTimeInfo,
      busScheduleAtStop: BusScheduleAtStop,
      $enabledFeatures: Signal[FeatureSets],
      namedRoute: NamedRoute,
      db: Persistence,
      componentSelector: Observer[ComponentData],
    ) = {

      def renderWaitTime(
        duration: MinuteDuration,
      ) =
        if (duration.toMinutes == 0)
          "Leaving!"
        else
          duration.toMinutes + " min."

      val modalActive = Var(false)
      val modalMode: Var[ModalMode] = Var(ModalMode.UpcomingStops)
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
            $enabledFeatures.map(
              _.isEnabled(Feature.BusAlarms),
            ),
            modalActive,
            modalMode,
            namedRoute,
            db,
            componentSelector,
          ),
        ),
      )
    }

    private def SafeRideLink(
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
  }

  def TripViewerLaminar(
    db: Persistence,
    componentSelector: Observer[ComponentData],
    initialTime: WallTime,
  ) =

    val $plan: Var[Plan] = Var(
      db.retrieveDailyPlanOnly.getOrElse(Plan(Seq.empty)),
    )
    div(
//      onMountCallback(_ => ),
      child <-- $plan.signal.map(plan =>
        div(
          div(
            button(
              cls := "button",
              "Delete saved plan",
              onClick --> db.saveDailyPlan(
                crestedbutte.Plan(Seq.empty),
                $plan,
              ),
            ),
            Components.PlanElement(plan, db, $plan, initialTime),
          ),
        ),
      ),
    )

  def smallStopSelector(
    namedRoute: NamedRoute,
    $plan: Var[Plan],
    db: Persistence,
    initialTime: WallTime,
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
        allStops.init.map(stop =>
          div(
            stop.location.name,
            onClick.mapTo(Some(stop.location)) --> startingPoint,
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
                  destination.name,
                  onClick --> Observer { _ =>
                    val start = startingPoint
                      .now()
                      .getOrElse(throw Exception("No starting point"))

                    val matchingLeg =
                      namedRoute.routeWithTimes.legs
                        .flatMap { leg =>
                          (for
                            trimmedToStart <- leg .trimToStartAt(start)
                            trimmedToEnd <- trimmedToStart.trimToEndAt(destination)
                            ends <- trimmedToEnd.ends
                          yield ends).toOption
                        }
                        .find { l =>
                          val lastArrivalTime =
                            $plan.now().legs.lastOption
                              .map(_.last.busTime)
                          val cutoff =
                            lastArrivalTime.getOrElse(initialTime)
                          l.head.busTime.isAfter(cutoff)
                        }
                        .getOrElse(
                          throw new Exception(
                            "Not route leg found with locations",
                          ),
                        )
                    $plan.update { case oldPlan =>
                      val newPlan =
                        oldPlan.copy(legs =
                          oldPlan.legs :+ matchingLeg,
                        )
                      db.saveDailyPlanOnly(newPlan)
                      newPlan
                    }
                  },
                ),
              ),
            )
          case None => div()
        },
      ),
    )
}
