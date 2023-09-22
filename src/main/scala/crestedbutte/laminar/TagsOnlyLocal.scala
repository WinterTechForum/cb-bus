package crestedbutte.laminar

import com.billding.time.{MinuteDuration, WallTime}
import com.raquo.laminar.nodes.ReactiveHtmlElement
import crestedbutte.NotificationStuff.desiredAlarms
import crestedbutte.*
import crestedbutte.dom.BulmaLocal
import crestedbutte.laminar.Experimental.getLocation
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

object TagsOnlyLocal {
  import com.raquo.laminar.api.L._

  def RouteLeg(
    routeLeg: RouteLeg,
  ) =
    div(
      routeLeg.stops.map(stop =>
        createBusTimeElementOnLeg(
          stop.location,
          div(
            stop.busTime.toDumbAmericanString,
          ),
        ),
      ),
    )

  def FullApp(
    pageMode: AppMode,
    initialRouteOpt: Option[String],
    javaClock: Clock,
  ) = {

    val clockTicks = new EventBus[Int]

    val components = AllRoutes.components(pageMode)

    val selectedRoute: Var[ComponentData] = Var(
      initialRouteOpt
        .flatMap(initialRoute =>
          components.find(
            _.componentName.elementNameMatches(initialRoute),
          ),
        )
        .getOrElse(
//          SpringFallLoop,
          TripPlannerComponent
//          RtaSouthbound.fullSchedule,
        ),
    )

    val timeStamps: Signal[WallTime] = clockTicks.events.foldLeft(
      WallTime(
        OffsetDateTime
          .now(javaClock)
          .toLocalTime
          .format(
            DateTimeFormatter.ofPattern("HH:mm"),
          ),
      ),
    )(
      (
        _,
        _,
      ) =>
        WallTime(
          OffsetDateTime
            .now(javaClock)
            .toLocalTime
            .format(
              DateTimeFormatter.ofPattern("HH:mm"),
            ),
        ),
    )

    div(
      Bulma.menu(selectedRoute, components),
      RepeatingElement()
        .repeatWithInterval( // This acts like a Dune thumper
          1,
          new FiniteDuration(20, scala.concurrent.duration.SECONDS),
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
    timeStamps: Signal[WallTime],
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

    val planner = LaminarTripPlanner
      .TripPlannerLaminar()

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
            case TripPlannerComponent => planner
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
      button(
        idAttr := "Get position",
        onClick --> Observer[dom.MouseEvent](
          onNext = ev => getLocation(gpsPosition),
        ),
        "Get GPS coords",
      ),
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
                namedRoute,
              ) =>
            TagsOnlyLocal.createBusTimeElement(
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
