package crestedbutte.laminar

import com.billding.time.{BusDuration, BusTime}
import com.raquo.laminar.nodes.ReactiveHtmlElement
import crestedbutte.NotificationStuff.desiredAlarms
import crestedbutte._
import crestedbutte.dom.BulmaLocal
import crestedbutte.routes.{AllRoutes, TownShuttleTimes}

import java.time.{Clock, OffsetDateTime}
import scala.concurrent.duration.FiniteDuration

object TagsOnlyLocal {
  import com.raquo.laminar.api.L._

  def FullApp(
    pageMode: AppMode,
    initialRouteOpt: Option[String],
    javaClock: Clock,
  ) = {

    val clockTicks = new EventBus[Int]

    val components = AllRoutes.components(pageMode)

    val selectedRoute: Var[ComponentData] = Var(
      initialRouteOpt
        .flatMap(
          initialRoute =>
            components.find(
              _.componentName.elementNameMatches(initialRoute),
            ),
        )
        .getOrElse(
          TownShuttleTimes,
        ),
    )

    val timeStamps: Signal[BusTime] = clockTicks.events.foldLeft(
      new BusTime(
        OffsetDateTime.now(javaClock).toLocalTime,
      ),
    )(
      (_, _) =>
        new BusTime(
          OffsetDateTime.now(javaClock).toLocalTime,
        ),
    )

    div(
      Bulma.menu(selectedRoute, components),
      RepeatingElement()
        .repeatWithInterval( // This acts like a Dune thumper
          1,
          new FiniteDuration(10, scala.concurrent.duration.SECONDS),
        ) --> clockTicks,
      TagsOnlyLocal
        .overallPageLayout(
          selectedRoute.signal,
          timeStamps,
          pageMode,
        ),
    )
  }

  def overallPageLayout(
    $selectedComponent: Signal[ComponentData],
    timeStamps: Signal[BusTime],
    pageMode: AppMode,
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
      $selectedComponent.combineWith(timeStamps).map {
        case (componentData, timestamp) =>
          componentData match {
            case RoundTripCalculatorComponent =>
              LaminarRoundTripCalculator
                .RoundTripCalculatorLaminar()
            case namedRoute: NamedRoute =>
              TagsOnlyLocal.structuredSetOfUpcomingArrivals(
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
      cls := "bill-box",
      idAttr := "container",
      child <-- upcomingArrivalData, // **THIS IS THE IMPORTANT STUFF** The fact that it's hard to see means I need to remove other bullshit
      timeStamps --> Observer[BusTime](
        onNext = localTime =>
          desiredAlarms
            .dequeueAll(_ => true)
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
    )
  }

  def renderWaitTime(
    duration: BusDuration,
  ) =
    if (duration.toMinutes == 0)
      "Leaving!"
    else
      duration.toMinutes + " min."

  def GeoBits(
    $mapLinksEnabled: Signal[Boolean],
    location: Location.Value,
    $gpsPosition: Signal[Option[GpsCoordinates]],
  ) =
    div(
      child <-- $mapLinksEnabled.map(
        mapLinksEnabled =>
          if (mapLinksEnabled)
            div(
              cls := "map-link",
              child <-- Components
                .distanceFromCurrentLocationToStop($gpsPosition,
                                                   location),
              location.gpsCoordinates.map(Components.GeoLink),
            )
          else
            div(),
      ),
    )

  def createBusTimeElement(
    location: Location.Value,
    content: ReactiveHtmlElement[_],
    $mapLinksEnabled: Signal[Boolean],
    $gpsPosition: Signal[Option[GpsCoordinates]], // TODO Should this be an `Option[Signal[GpsCoordinates]` instead?
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

  def renderStopTimeInfo(
    stopTimeInfo: StopTimeInfo,
    busScheduleAtStop: BusScheduleAtStop,
    $enabledFeatures: Signal[FeatureSets],
  ) = {
    val modalActive = Var(false)
    div(
      button(
        cls := "arrival-time button open-arrival-time-modal",
        onClick.preventDefault.map(_ => {
          org.scalajs.dom.document
            .querySelector("html")
            .classList
            .add("is-clipped")
          true
        }) --> modalActive,
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
        ),
      ),
    )
  }

  def RouteHeader(
    routeName: RouteName,
  ) =
    div(
      cls := "route-header",
      span(
        cls := "route-header_name",
        routeName.userFriendlyName + " Departures",
      ),
      Components.SvgIcon("glyphicons-basic-32-bus.svg"),
    )

  def structuredSetOfUpcomingArrivals(
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
              ) =>
            TagsOnlyLocal.createBusTimeElement(
              location,
              content match {
                case Left(stopTimeInfo) =>
                  renderStopTimeInfo(
                    stopTimeInfo,
                    fullScheduleAtStop,
                    $enabledFeatures,
                  )
                case Right(safeRideRecommendation) =>
                  Components.SafeRideLink(safeRideRecommendation)
              },
              $enabledFeatures.map(
                _.isEnabled(Feature.MapLinks),
              ),
              gpsPosition.signal,
            )
        },
    )

}
