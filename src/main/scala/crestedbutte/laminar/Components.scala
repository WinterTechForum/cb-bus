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
import org.scalajs.dom.{
  window,
  IDBDatabase,
  IDBEvent,
  IDBTransactionMode,
  IDBValue,
}

object Components {
  def GPS(
    gpsPosition: Var[Option[GpsCoordinates]],
  ) =
    button(
      idAttr := "Get position",
      onClick --> Observer[dom.MouseEvent](
        onNext = ev => getLocation(gpsPosition),
      ),
      "Get GPS coords",
    )

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

  def SvgIcon(
    name: String,
  ) =
    img(
      cls := "glyphicon",
      src := s"/glyphicons/svg/individual-svg/$name",
      alt := "Thanks for riding the bus!",
    )

  implicit val location2selectorValue: Location => SelectValue =
    (location: Location) => SelectValue(location.name, location.name)

  def StopSelector(
    label: String,
    $selection: Var[Location],
    $currentRoute: Var[NamedRoute],
  ) =
    div(
      label,
      child <--
        $currentRoute.signal
          .map(_.allStops)
          .map(route =>
            Selector(
              route,
              $selection,
            ),
          ),
    )

  case class SelectValue(
    uniqueValue: String,
    humanFriendlyName: String)

  // TODO Lot of ugly code to work through in this method
  def Selector[T](
    route: Seq[T],
    stopSelection: Var[T],
  )(implicit converterThatCouldBeATypeClass: T => SelectValue,
  ) = {

    val valueMap: Map[SelectValue, T] =
      route
        .map(selectValue =>
          (converterThatCouldBeATypeClass(selectValue), selectValue),
        )
        .toMap
    val selectValues = route.map(converterThatCouldBeATypeClass)
    span(
      cls := "select is-rounded",
      select(
        inContext { thisNode =>
          onChange
            .mapTo(thisNode.ref.value)
            .map(uniqueValue =>
              selectValues
                .find(_.uniqueValue == uniqueValue)
                .get,
            )
            .map(
              valueMap.getOrElse(_,
                                 throw new RuntimeException(
                                   "can't find the value!",
                                 ),
              ),
            ) --> stopSelection.writer
        },
        selectValues.map(stop =>
          option(selected := valueMap(stop) == stopSelection.now(),
                 value(stop.uniqueValue),
                 stop.humanFriendlyName,
          ),
        ),
      ),
    )
  }

  def RouteSelector(
    $currentRoute: Var[NamedRoute],
    $startingPoint: Var[Location],
    $destination: Var[Location],
  ) =
    val fullRouteAndStopsUpdater =
      Observer[NamedRoute](
        onNext = route => {
          $startingPoint.set($destination.now())
          $destination.set(route.allStops.last)
          $currentRoute.update(_ => route)
        },
      )
    div(
      cls := "control",
      label(
        cls := "radio",
        input(
          typ := "radio",
          nameAttr := "routeSelection",
          onClick.mapTo(
            RtaNorthbound.fullSchedule,
          ) --> fullRouteAndStopsUpdater,
        ),
        RtaNorthbound.fullSchedule.routeName.userFriendlyName,
      ),
      label(
        cls := "radio",
        input(
          typ := "radio",
          nameAttr := "routeSelection",
          defaultChecked := true,
          onClick.mapTo(
            RtaSouthbound.fullSchedule,
          ) --> fullRouteAndStopsUpdater,
        ),
        RtaSouthbound.fullSchedule.routeName.userFriendlyName,
      ),
    )

  def TripBoundarySelector(
    $tripBoundary: Var[TripBoundary],
  ) =
    div(
      cls := "control",
      label(
        cls := "radio",
        input(
          typ := "radio",
          nameAttr := "tripBoundarySelection",
          defaultChecked := $tripBoundary.now() == TripBoundary.ArrivingBy,
          onClick.mapTo(TripBoundary.ArrivingBy) --> $tripBoundary,
        ),
        "Arriving By",
      ),
      label(
        cls := "radio",
        input(
          typ := "radio",
          nameAttr := "tripBoundarySelection",
          defaultChecked := $tripBoundary.now() == TripBoundary.StartingAfter,
          onClick.mapTo(TripBoundary.StartingAfter) --> $tripBoundary,
        ),
        "Leaving After",
      ),
    )

  def RouteLegElement(
    label: String,
    routeLeg: RouteLeg,
  ) =
    div(
//      routeLeg.stops.head. + " to ",

      routeLeg.stops.head match

        case LocationWithTime(
              location,
              busTime,
            ) =>
          createBusTimeElementOnLeg(
            location,
            div(
              busTime.toDumbAmericanString,
            ),
          )
      ,
      div(
        cls := "scrollable-route-leg",
        routeLeg.stops.tail.map(stop =>
          createBusTimeElementOnLeg(
            stop.location,
            div(
              button(
                cls := "button",
                onClick.mapTo(
                  RouteLeg(Seq(routeLeg.stops.head, stop)),
                ) --> Observer { (e: RouteLeg) =>

                  val plan = Plan(Seq(e))
                  dom.window.navigator.clipboard
                    .writeText(plan.plainTextRepresentation)
                  println("Should copy this to paste buffer: " + e)
                },
                "+",
              ),
              div(
                stop.busTime.toDumbAmericanString,
              ),
            ),
          ),
        ),
      ),
    )

  import com.raquo.laminar.nodes.ReactiveHtmlElement

  def RouteLegEnds(
    routeLeg: RouteLeg,
    $plan: Var[Plan],
  ) =
    div(
      div(
        button(
          cls := "button",
          "Add to Plan",
          onClick --> Observer { _ =>
            $plan.update(plan =>
              plan.copy(legs = plan.legs :+ routeLeg.ends),
            )
            println("New plan: " + $plan.now())
          },
        ),
      ),
      div:
        List(
          routeLeg.stops.head,
          routeLeg.stops.last,
        ).map: stop =>
          createBusTimeElementOnLeg(
            stop.location,
            div:
              stop.busTime.toDumbAmericanString,
          ),
    )

  import org.scalajs.dom.{
    window,
    IDBDatabase,
    IDBEvent,
    IDBTransactionMode,
    IDBValue,
  }
  import crestedbutte.pwa.Persistence
  def PlanElement(
    plan: Plan,
    db: Var[Option[IDBDatabase]],
  ) =
    if (plan.legs.nonEmpty)
      div(
        "Plan: ",
        button(
          cls := "button",
          "Copy to Clipboard",
          onClick --> Persistence.saveDailyPlan(plan, db),
          onClick --> Observer { _ =>
            dom.window.navigator.clipboard
              .writeText(plan.plainTextRepresentation)
          },
        ),
        plan.legs.zipWithIndex.map { case (routeLeg, idx) =>
          div(
            RouteLegElement(
              "Trip " + (idx + 1),
              routeLeg,
            ),
          )
        },
      )
    else div()

  import com.raquo.laminar.api.L._

  def FullApp(
    pageMode: AppMode,
    initialComponent: Option[ComponentName],
    javaClock: Clock,
  ) = {
    val db: Var[Option[IDBDatabase]] = Var(
      None,
    )

    val clockTicks = new EventBus[Int]

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
//          TripPlannerComponent,
          RtaSouthbound.fullSchedule,
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
      .roundToNextFiveMutable()

    val timeStamps: Signal[WallTime] = clockTicks.events
      .filter(_ =>
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
        Persistence.createDb(db),
      Bulma.menu(selectedComponent, components),
      RepeatingElement()
        .repeatWithInterval( // This acts like a Dune thumper
          1,
          new FiniteDuration(5, scala.concurrent.duration.SECONDS),
        ) --> clockTicks,
      overallPageLayout(
        selectedComponent.signal,
        timeStamps,
        pageMode,
        initialTime,
        db,
      ),
    )
  }

  def overallPageLayout(
    $selectedComponent: Signal[ComponentData],
    timeStamps: Signal[WallTime],
    pageMode: AppMode,
    initialTime: WallTime,
    db: Var[Option[IDBDatabase]],
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

    val planner = LaminarTripPlanner
      .TripPlannerLaminar(initialTime, db)

    val upcomingArrivalData =
      $selectedComponent.combineWith(timeStamps)
        .map { case (componentData, timestamp) =>
          // This is a super janky way to avoid being unable to scroll
          // after we refresh the page and close the model
          org.scalajs.dom.document
            .querySelector("html")
            .classList
            .remove("is-clipped")
          componentData match {
            case PlanViewer           => ???
            case TripPlannerComponent => planner
            case namedRoute: NamedRoute =>
              TopLevelRouteView(
                TimeCalculations
                  .getUpComingArrivalsWithFullScheduleNonZio(
                    timestamp,
                    namedRoute,
                  ),
                $enabledFeatures,
                gpsPosition,
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

  def renderWaitTime(
    duration: MinuteDuration,
  ) =
    if (duration.toMinutes == 0)
      "Leaving!"
    else
      duration.toMinutes + " min."

  def GeoBits(
    $mapLinksEnabled: Signal[Boolean],
    location: Location,
    $gpsPosition: Signal[Option[GpsCoordinates]],
  ) =
    div(
      child <-- $mapLinksEnabled.map(mapLinksEnabled =>
        if (mapLinksEnabled)
          div(
            cls := "map-link",
            child <-- Components
              .distanceFromCurrentLocationToStop($gpsPosition,
                                                 location,
              ),
            location.gpsCoordinates.map(Components.GeoLink),
          )
        else
          div(),
      ),
    )

  def createBusTimeElement(
    location: Location,
    content: ReactiveHtmlElement[_],
    $mapLinksEnabled: Signal[Boolean],
    $gpsPosition: Signal[
      Option[GpsCoordinates],
    ], // TODO Should this be an `Option[Signal[GpsCoordinates]` instead?
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

  // TODO Dedup with above
  def createBusTimeElementOnLeg(
    location: Location,
    content: ReactiveHtmlElement[_],
  ) =
    div(
      width := "100%",
      cls := "stop-information",
      div(cls := "stop-name", div(location.name)),
      div(cls := "stop-alt-name", div(location.altName)),
      div(cls := "upcoming-information", content),
    )

  def StopTimeInfoForLocation(
    stopTimeInfo: StopTimeInfo,
    busScheduleAtStop: BusScheduleAtStop,
    $enabledFeatures: Signal[FeatureSets],
    namedRoute: NamedRoute,
  ) = {
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
        ),
      ),
    )
  }

  def RouteHeader(
    routeName: ComponentName,
  ) =
    div(
      cls := "route-header",
      span(
        cls := "route-header_name",
        routeName.userFriendlyName + " Departures",
      ),
      Components.SvgIcon("glyphicons-basic-32-bus.svg"),
    )

  def TopLevelRouteView(
    upcomingArrivalComponentData: UpcomingArrivalComponentData,
    $enabledFeatures: Signal[FeatureSets],
    gpsPosition: Var[Option[GpsCoordinates]],
  ) =
    div(
      RouteHeader(upcomingArrivalComponentData.routeName),
      upcomingArrivalComponentData.upcomingArrivalInfoForAllRoutes
        .map {
          case UpcomingArrivalInfoWithFullSchedule(
                UpcomingArrivalInfo(location, content),
                fullScheduleAtStop,
                namedRoute,
              ) =>
            createBusTimeElement(
              location,
              content match {
                case Left(stopTimeInfo) =>
                  StopTimeInfoForLocation(
                    stopTimeInfo,
                    fullScheduleAtStop,
                    $enabledFeatures,
                    namedRoute,
                  )
                case Right(safeRideRecommendation) =>
                  Components.SafeRideLink(safeRideRecommendation)
              },
              $enabledFeatures.map(
                //                _.isEnabled(Feature.MapLinks),
                _ => true, // Maps always enabled now
              ),
              gpsPosition.signal,
            )
        },
    )
}
