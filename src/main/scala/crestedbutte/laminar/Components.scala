package crestedbutte.laminar

import com.raquo.laminar.api.L._
import crestedbutte.{
  Feature,
  GpsCalculations,
  GpsCoordinates,
  Location,
}

object Components {

  def distanceBetween(
    gpsPosition: Signal[Option[GpsCoordinates]],
    location: Location.Value,
  ) =
    gpsPosition.map(
      gpsCoordsOpt =>
        gpsCoordsOpt
          .flatMap(
            userCords =>
              location.gpsCoordinates.map(
                stopCoords =>
                  div(
                    GpsCalculations
                      .distanceInKmBetweenEarthCoordinatesT(
                        userCords,
                        stopCoords,
                      ),
                  ),
              ),
          )
          .getOrElse(div()),
    )

  def FeatureControlCenter(
    featureUpdates: WriteBus[FeatureStatus],
  ) = {

    // TODO Make this a separate component?
    def featureToggle(
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
      Feature.values.map(featureToggle),
    )
  }
}
