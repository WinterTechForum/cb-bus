package crestedbutte.laminar

import com.raquo.laminar.api.L.*
import crestedbutte.*
import crestedbutte.routes.{RtaNorthbound, RtaSouthbound}

object Components {

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

  def RouteSelector($currentRoute: Var[NamedRoute]) =
    div(cls := "control",
      label(cls := "radio",
        input(typ := "radio", nameAttr := "routeSelection",
          onClick.mapTo(RtaNorthbound.fullSchedule) --> $currentRoute
        ),
        RtaNorthbound.fullSchedule.routeName.userFriendlyName
      ),
      label(cls := "radio",
        input(typ := "radio", nameAttr := "routeSelection", defaultChecked := true,
          onClick.mapTo(RtaSouthbound.fullSchedule) --> $currentRoute
        ),
        RtaSouthbound.fullSchedule.routeName.userFriendlyName
      )
    )

  def TripBoundarySelector($tripBoundary: Var[TripBoundary]) =
    div(cls := "control",
      label(cls := "radio",
        input(typ := "radio", nameAttr := "tripBoundarySelection", defaultChecked := $tripBoundary.now() == TripBoundary.ArrivingBy,
          onClick.mapTo(TripBoundary.ArrivingBy) --> $tripBoundary
        ),
        "Arriving By"
      ),
      label(cls := "radio",
        input(typ := "radio", nameAttr := "tripBoundarySelection", defaultChecked := $tripBoundary.now() == TripBoundary.StartingAfter,
          onClick.mapTo(TripBoundary.StartingAfter) --> $tripBoundary
        ),
        "Leaving After"
      )
    )
}
