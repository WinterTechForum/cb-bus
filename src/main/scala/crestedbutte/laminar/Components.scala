package crestedbutte.laminar

import com.billding.time.BusTime
import com.raquo.laminar.api.L._
import crestedbutte.NotificationStuff.desiredAlarms
import crestedbutte.laminar.TagsOnlyLocal.svgIcon
import crestedbutte.{
  Feature,
  FeatureStatus,
  GpsCalculations,
  GpsCoordinates,
  LateNightRecommendation,
  Location,
  NotificationStuff,
}
import org.scalajs.dom.experimental.{
  Notification,
  NotificationOptions,
}

import scala.scalajs.js

object Components {

  def distanceBetween(
    gpsPosition: Signal[Option[GpsCoordinates]],
    location: Location.Value,
  ) =
    gpsPosition.map(
      _.flatMap(
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
      ).getOrElse(div()),
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

  def GeoLink(
    gpsCoordinates: GpsCoordinates,
  ) =
    a(
      cls := "link",
      href := s"https://www.google.com/maps/search/?api=1&query=${gpsCoordinates.latitude},${gpsCoordinates.longitude}",
      svgIcon("glyphicons-basic-592-map.svg"),
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
          img(
            cls := "glyphicon",
            src := "/glyphicons/svg/individual-svg/glyphicons-basic-465-call.svg",
            alt := "Call Late Night Shuttle!",
          ),
          safeRideRecommendation.message,
        ),
      ),
    )

}
