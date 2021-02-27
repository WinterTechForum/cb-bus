package crestedbutte

import crestedbutte.Location.StopLocation
import crestedbutte.dom.{Bulma, BulmaLocal}
import com.billding.time.BusTime
import com.billding.time.BusDuration
import com.raquo.laminar.nodes.ReactiveHtmlElement
import crestedbutte.laminar.{
  LaminarRoundTripCalculator,
  RepeatingElement,
}
import crestedbutte.routes.TownShuttleTimes

import java.time.LocalTime
import java.time.Clock
import scala.concurrent.duration.FiniteDuration

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

    div(
      cls := "bill-box",
      idAttr := "container",
      child <-- upcomingArrivalData,
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
          button(
            idAttr := ElementNames.Notifications.requestPermission,
            cls := "button",
            "Request Notifications Permission",
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
      div(
        cls := "map-link",
        // TODO Re-enable once maps are more polished
        //  geoLinkForStop(location)
      ),
      div(cls := "stop-name", div(location.name)),
      div(cls := "stop-alt-name", div(location.altName)),
      div(cls := "upcoming-information", content),
    )

  def geoLinkForStop(
    stopLocation: StopLocation,
  ) =
    a(
      cls := "link",
      //    <a href="geo:37.786971,-122.399677;u=35">open map</a>
//          href := s"geo:${stopLocation.gpsCoordinates.latitude}, ${stopLocation.gpsCoordinates.longitude}"
      href := s"https://www.google.com/maps/search/?api=1&query=${stopLocation.gpsCoordinates.latitude},${stopLocation.gpsCoordinates.longitude}",
    )(svgIcon("glyphicons-basic-592-map.svg"))

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
  ) =
    img(
      cls := "glyphicon " + classes,
      src := s"/glyphicons/svg/individual-svg/$name",
      alt := "Thanks for riding the bus!",
      dataAttr("lossless-value") := busTime.toString,
      verticalAlign := "middle",
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
