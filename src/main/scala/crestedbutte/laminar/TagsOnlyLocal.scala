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
    pageMode: AppMode.Value,
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
    pageMode: AppMode.Value,
  ) = {
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
        onNext = localTime => {
          desiredAlarms
            .dequeueAll(_ => true)
            .map(
              Experimental.Notifications
                .createJankyBusAlertInSideEffectyWay(_, localTime),
            )
        },
      ),
      Option.when(pageMode == AppMode.Development)(
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
      child <-- $mapLinksEnabled.map(
        mapLinksEnabled =>
          if (mapLinksEnabled)
            div(
              cls := "map-link",
              child <-- Components
                .distanceFromCurrentLocationToStop($gpsPosition, location),
              location.gpsCoordinates.map(Components.GeoLink),
            )
          else
            div(),
      ),
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
            enabledFeatures =>
              enabledFeatures.isEnabled(Feature.BusAlarms),
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
      img(
        cls := "glyphicon route-header_icon",
        src := "/glyphicons/svg/individual-svg/glyphicons-basic-32-bus.svg",
        alt := "Thanks for riding the bus!",
      ),
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
                enabledFeatures =>
                  enabledFeatures.isEnabled(Feature.MapLinks),
              ),
              gpsPosition.signal,
            )
        },
    )

  def svgIcon(
    name: String,
  ) =
    img(
      cls := "glyphicon",
      src := s"/glyphicons/svg/individual-svg/$name",
      alt := "Thanks for riding the bus!",
    )
  /*
  glyphicons-basic-591-map-marker.svg
  glyphicons-basic-417-globe.svg
  glyphicons-basic-262-direction-empty.svg
  glyphicons-basic-581-directions.svg
  glyphicons-basic-697-directions-sign.svg

 */

}
