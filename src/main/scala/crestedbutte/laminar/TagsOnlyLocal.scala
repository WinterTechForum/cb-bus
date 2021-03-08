package crestedbutte.laminar

import com.billding.time.{BusDuration, BusTime}
import com.raquo.laminar.nodes.ReactiveHtmlElement
import crestedbutte.NotificationStuff.desiredAlarms
import crestedbutte.dom.BulmaLocal
import crestedbutte._
import crestedbutte.routes.{AllRoutes, TownShuttleTimes}
import org.scalajs.dom.experimental.{
  Notification,
  NotificationOptions,
}

import java.time.{Clock, OffsetDateTime}
import scala.concurrent.duration.FiniteDuration
import scala.scalajs.js

object TagsOnlyLocal {
  import com.raquo.laminar.api.L._

  def createPopupContent(
    scheduleAtStop: BusScheduleAtStop,
  ) =
    div(
      div(
        idAttr := s"popup_${scheduleAtStop.location}",
        cls := "overlay light",
        a(cls := "cancel", href := "#", div("x" /*&times*/ )),
        div(cls := "popup",
            h2("Later Arrivals"),
            div(cls := "content",
                div(
                  scheduleAtStop.times.map(
                    time =>
                      div(
                        span(time.toDumbAmericanString),
                      ),
                  ),
                ),
            ),
        ),
      ),
    )

  def hamburgerMenu() =
    a(role := "button",
      cls := "navbar-burger",
      aria.label := "menu",
      aria.expanded := false,
      span(aria.hidden := true),
      span(aria.hidden := true),
      span(aria.hidden := true),
    )

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
          ComponentDataRoute(
            TownShuttleTimes,
          ),
        ),
    )

    val timeStamps: Signal[BusTime] = clockTicks.events.foldLeft(
      new BusTime(
        OffsetDateTime.now(javaClock).toLocalTime,
      ),
    )(
      (oldTime, _) =>
        new BusTime(
          OffsetDateTime.now(javaClock).toLocalTime,
        ),
    )

    div(
      Bulma.menu(selectedRoute, components),
      RepeatingElement().repeatWithInterval(
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
        case (route, timestamp) =>
          println("acting on selectedComponent update!")
          route match {
            case ComponentDataTyped(value, componentName) =>
              LaminarRoundTripCalculator
                .RoundTripCalculatorLaminar()
            case ComponentDataRoute(namedRoute) =>
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
              Experimental
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

  def busScheduleDiv(
    containerName: String,
  ) = {
    println("Making container : " + containerName)
    div(cls := ElementNames.BoxClass,
        idAttr := containerName,
        div(cls := "timezone"),
        div(idAttr := ElementNames.contentName),
    )
  }

  def safeRideLink(
    safeRideRecommendation: LateNightRecommendation,
  ) =
    div(
      cls := "late-night-call-button",
      a(
        href := s"tel:${safeRideRecommendation.phoneNumber}",
        cls := "link",
        button(
          cls := "button",
          img(
            cls := "glyphicon",
            src := "/glyphicons/svg/individual-svg/glyphicons-basic-465-call.svg",
            alt := "Call Late Night Shuttle!",
          ),
          safeRideRecommendation.message,
        ),
      ),
    )

  // TODO Redundant with above saferide link?
  def phoneLink(
    phoneNumber: PhoneNumber,
  ) =
    a(href := s"tel:${phoneNumber.number}",
      cls := "link",
      phoneNumber.name,
    )

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
    $gpsPosition: Var[Option[GpsCoordinates]],
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
                .distanceBetween($gpsPosition.signal, location),
              location.gpsCoordinates.map(geoLinkForStop),
            )
          else
            div(),
      ),
      div(cls := "stop-name", div(location.name)),
      div(cls := "stop-alt-name", div(location.altName)),
      div(cls := "upcoming-information", content),
    )

  def geoLinkForStop(
    gpsCoordinates: GpsCoordinates,
  ) =
    a(
      cls := "link",
      //    <a href="geo:37.786971,-122.399677;u=35">open map</a>
//          href := s"geo:${stopLocation.gpsCoordinates.latitude}, ${stopLocation.gpsCoordinates.longitude}"
      href := s"https://www.google.com/maps/search/?api=1&query=${gpsCoordinates.latitude},${gpsCoordinates.longitude}",
      svgIcon("glyphicons-basic-592-map.svg"),
    )

  def activateModal(
    targetName: String,
  ): Unit =
    org.scalajs.dom.document.body
      .querySelector(targetName)
      .classList
      .add("is-active")

  def modalContentElementNameTyped(
    location: Location.Value,
    routeName: RouteName,
  ) =
    dataAttr("schedule-modal") := modalContentElementName(location,
                                                          routeName)

  def modalContentElementName(
    location: Location.Value,
    routeName: RouteName,
  ) =
    "modal_content_" + routeName.name + "_" + location.elementName

  def renderStopTimeInfo(
    stopTimeInfo: StopTimeInfo,
    busScheduleAtStop: BusScheduleAtStop,
    routeName: RouteName,
    $enabledFeatures: Signal[FeatureSets],
  ) = {
    val modalActive = Var(false)
    div(
      button(
        cls := "arrival-time button open-arrival-time-modal",
        modalContentElementNameTyped(
          busScheduleAtStop.location,
          routeName,
        ),
        onClick.preventDefault.map(_ => {
          org.scalajs.dom.document
            .querySelector("html")
            .classList
            .add("is-clipped")
          println("Clicked modal open")
          true
        }) --> modalActive,
        stopTimeInfo.time.toDumbAmericanString,
      ),
      div(
        cls := "wait-time",
        renderWaitTime(stopTimeInfo.waitingDuration),
        BulmaLocal.bulmaModal(
          busScheduleAtStop,
          modalContentElementName(busScheduleAtStop.location,
                                  routeName),
          $enabledFeatures.map(
            enabledFeatures =>
              enabledFeatures.isEnabled(Feature.BusAlarms),
          ),
          modalActive,
        ),
      ),
    )
  }

  def structuredSetOfUpcomingArrivals(
    upcomingArrivalComponentData: UpcomingArrivalComponentData,
    $enabledFeatures: Signal[FeatureSets],
    gpsPosition: Var[Option[GpsCoordinates]],
  ) =
    div(
      div(
        cls := "route-header",
        span(
          cls := "route-header_name",
          upcomingArrivalComponentData.routeName.userFriendlyName + " Departures",
        ),
        img(
          cls := "glyphicon route-header_icon",
          src := "/glyphicons/svg/individual-svg/glyphicons-basic-32-bus.svg",
          alt := "Thanks for riding the bus!",
        ),
      ),
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
                    upcomingArrivalComponentData.routeName,
                    $enabledFeatures,
                  )
                // TODO turn this into a feature?
                case Right(safeRideRecommendation) =>
                  safeRideLink(safeRideRecommendation)
              },
              $enabledFeatures.map(
                enabledFeatures =>
                  enabledFeatures.isEnabled(Feature.MapLinks),
              ),
              gpsPosition,
            )
        },
    )

  def svgIconForAlarm(
    name: String,
    classes: String,
    busTime: BusTime,
  ) = {
    val clickObserverNarrow = Observer[BusTime](
      onNext = ev => {
        // This will give the user an idea of what the eventual notification will look/sound like
        // While also letting them know that they successfully scheduled it.
        new Notification(
          s"You will be alerted with a Notification like this when the bus is ${NotificationStuff.headsUpAmount.toMinutes} minutes away.",
          NotificationOptions(
            vibrate = js.Array(100d),
          ),
        )
        desiredAlarms.append(ev)
      },
    )
    img(
      cls := "glyphicon " + classes,
      src := s"/glyphicons/svg/individual-svg/$name",
      alt := "Thanks for riding the bus!",
      verticalAlign := "middle",
      onClick.map(_ => busTime) --> clickObserverNarrow,
    )
  }

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
