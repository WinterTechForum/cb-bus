package crestedbutte.laminar

import com.billding.time.{BusDuration, BusTime}
import com.raquo.laminar.nodes.ReactiveHtmlElement
import crestedbutte.{
  BusScheduleAtStop,
  ElementNames,
  Feature,
  GpsCoordinates,
  LateNightRecommendation,
  Location,
  NotificationStuff,
  PhoneNumber,
  RouteName,
  StopTimeInfo,
  TimeCalculations,
  UpcomingArrivalComponentData,
  UpcomingArrivalInfo,
  UpcomingArrivalInfoWithFullSchedule,
}
import crestedbutte.Location.StopLocation
import crestedbutte.NotificationStuff.{desiredAlarms, headsUpAmount}
import crestedbutte.dom.BulmaLocal
import crestedbutte.routes.TownShuttleTimes
import org.scalajs.dom
import org.scalajs.dom.experimental.{
  Notification,
  NotificationOptions,
}

import java.time.Clock
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

  def overallPageLayout(
    clock: Clock,
    $selectedComponent: Signal[ComponentData],
    timeStamps: Signal[BusTime],
    pageMode: AppMode.Value,
    allComponentData: Seq[ComponentData],
  ) = {

    val initialArrivalsAtAllRouteStops = TimeCalculations
      .getUpComingArrivalsWithFullScheduleNonZio(
        clock,
        TownShuttleTimes,
      )

    val clickObserver = Observer[dom.MouseEvent](
      onNext = ev => {
        if (Notification.permission == "default")
          Notification.requestPermission(
            response =>
              println(
                "Notification requestPermission response: " + response,
              ),
          )
        else if (Notification.permission == "denied")
          println(
            "They denied permission to notifications. Give it up.",
          )
        else if (Notification.permission == "granted")
          println("we already have permission.")
        else
          println(
            "Uknown permission state: " + Notification.permission,
          )
      },
    )

    val upcomingArrivalData =
      $selectedComponent.combineWith(timeStamps).foldLeft(
        _ =>
          TagsOnlyLocal.structuredSetOfUpcomingArrivals(
            initialArrivalsAtAllRouteStops,
          ),
      ) {
        case (_, (route, timestamp)) => { // TODO Start using timestamp below, to avoid passing clock where it's not needed
          println("acting on selectedComponent update!")
          route match {
            case ComponentDataTyped(value, componentName) =>
              LaminarRoundTripCalculator
                .RoundTripCalculatorLaminar()
            case ComponentDataRoute(namedRoute) =>
              TagsOnlyLocal.structuredSetOfUpcomingArrivals(
                TimeCalculations
                  .getUpComingArrivalsWithFullScheduleNonZio(
                    clock,
                    namedRoute,
                  ),
              )
          }
        }
      }

    case class FeatureStatus(
      feature: Feature,
      enabled: Boolean)

    case class FeatureSets(
      values: Map[Feature, Boolean]) {
      def isEnabled(
        feature: Feature,
      ): Boolean =
        values(feature) // Unsafe
      def update(
        featureStatus: FeatureStatus,
      ): FeatureSets =
        copy(
          values = values + (kv =
              (featureStatus.feature, featureStatus.enabled)),
        )
    }

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

    div(
      cls := "bill-box",
      idAttr := "container",
      child <-- upcomingArrivalData,
      timeStamps --> Observer[BusTime](
        onNext = localTime => {
          println(s"Checking for alarms at: $localTime")
          val busTimes = desiredAlarms.dequeueAll {
            _ =>
              true
          }
          busTimes.map {
            busTime =>
              if (localTime
                    .between(busTime)
                    // TODO Direct comparison
                    .toMinutes >= headsUpAmount.toMinutes)
                println("1")
              dom.window.setTimeout(
                // TODO Replace this with submission to an EventBus[BusTime] that can be read via the RepeatingElement
                () =>
                  // Read submitted time, find difference between it and the current time, then submit a setInterval function
                  // with the appropriate delay
                  new Notification(
                    s"The ${busTime.toString} bus is arriving in ${headsUpAmount.toMinutes} minutes!",
                    NotificationOptions(
                      vibrate = js.Array(100d),
                    ),
                  ),
                (localTime
                  .between(busTime)
                  .toMinutes - headsUpAmount.toMinutes) * 60 * 1000,
              )
              println("2")
          }
        },
      ),
      /*
      Restore once I have a Laminar-friendly Bulma
      Bulma.menu(
        allComponentData.map {
          componentData =>
            println("should be creating a route menu entry")
            a(
              cls := "navbar-item",
              dataAttr("route") := componentData.componentName.name,
            componentData.componentName.userFriendlyName)
        },
        "Routes",
        "route  using-library",
      ),
       */

      // TODO Should this just go away?
//      allComponentData.map(
//        (singleComponentData: ComponentData) =>
//          singleComponentData match {
//            case ComponentDataTyped(value, componentName) =>
//              LaminarRoundTripCalculator.RoundTripCalculatorLaminar()
//            case ComponentDataRoute(namedRoute) =>
//              busScheduleDiv(singleComponentData.componentName.name)
//          },
//      ),
      if (pageMode == AppMode.Development) {
        div(
          label(
            cls := "checkbox",
            "Map Links",
            input(
              typ := "checkbox",
              onInput.mapToChecked.map(
                FeatureStatus(Feature.MapLinks, _),
              ) --> featureUpdates,
              onInput.mapToChecked --> Observer[Boolean](
                onNext =
                  isChecked => println("isChecked: " + isChecked),
              ),
            ),
          ),
          div(
            child <-- $enabledFeatures.map(
              enabledFeatures =>
                pprint.apply(enabledFeatures).toString,
            ),
          ),
          button(
            idAttr := ElementNames.Notifications.requestPermission,
            cls := "button",
            "Request Notifications Permission",
            onClick --> clickObserver,
          ),
          button(
            idAttr := ElementNames.Notifications.submitMessageToServiceWorker,
            cls := "button",
            "SubmitMessage to SW",
          ),
        )
      }
      else div(),
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

  //  <a href="tel:123-456-7890">123-456-7890</a>
  def safeRideLink(
    safeRideRecommendation: LateNightRecommendation,
  ) =
    div(
      cls := "late-night-call-button",
      button(
        // TODO restore
//        onClick := s"window.location.href = 'tel:${safeRideRecommendation.phoneNumber}';",
        cls := "button",
        img(
          cls := "glyphicon",
          src := "/glyphicons/svg/individual-svg/glyphicons-basic-465-call.svg",
          alt := "Call Late Night Shuttle!",
        ),
        safeRideRecommendation.message,
      ),
    )

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
    /* TODO: waitDuration: Duration*/
  ) =
    div(
      width := "100%",
      cls := "stop-information",
      // TODO Put behind dev flag. Or maybe I should have a Premium flag?
      div(
        cls := "map-link",
        location.gpsCoordinates.map(geoLinkForStop),
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
  ) =
    div(
      button(
        cls := "arrival-time button open-arrival-time-modal",
        modalContentElementNameTyped(
          busScheduleAtStop.location,
          routeName,
        ),
//          data("schedule-modal") := modalContentElementName(
//            busScheduleAtStop.location,
//            routeName
//          ),
//        onClick := {},
//          s"activateModal('#popup_${busScheduleAtStop.location}');",
        dataAttr("lossless-value") := stopTimeInfo.time.toString,
        stopTimeInfo.time.toDumbAmericanString,
      ),
      div(
        cls := "wait-time",
        renderWaitTime(stopTimeInfo.waitingDuration),
        // TODO Restore Laminar-friendly modal
        BulmaLocal.bulmaModal(
          busScheduleAtStop,
          modalContentElementName(busScheduleAtStop.location,
                                  routeName),
        ),
      ),
    )

  def structuredSetOfUpcomingArrivals(
    upcomingArrivalComponentData: UpcomingArrivalComponentData,
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
                  )
                case Right(safeRideRecommendation) =>
                  safeRideLink(safeRideRecommendation)
              },
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
