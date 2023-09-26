package crestedbutte.laminar

import com.raquo.laminar.api.L.*
import crestedbutte.*
import crestedbutte.routes.{RtaNorthbound, RtaSouthbound}
import org.scalajs.dom
import crestedbutte.laminar.Experimental.getLocation

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
          .map(
            route => Selector(
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
    eventStream: Var[T],
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
            ) --> eventStream.writer
        },
        selectValues.map(stop =>
          option(selected := valueMap(stop) == eventStream.now(), value(stop.uniqueValue), stop.humanFriendlyName),
        ),
      ),
    )
  }

  def RouteSelector(
    $currentRoute: Var[NamedRoute],
  ) =
    div(
      cls := "control",
      label(
        cls := "radio",
        input(
          typ := "radio",
          nameAttr := "routeSelection",
          onClick.mapTo(RtaNorthbound.fullSchedule) --> $currentRoute,
        ),
        RtaNorthbound.fullSchedule.routeName.userFriendlyName,
      ),
      label(
        cls := "radio",
        input(
          typ := "radio",
          nameAttr := "routeSelection",
          defaultChecked := true,
          onClick.mapTo(RtaSouthbound.fullSchedule) --> $currentRoute,
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
}
