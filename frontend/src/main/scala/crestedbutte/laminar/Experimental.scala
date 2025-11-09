package crestedbutte.laminar

import com.billding.time.WallTime
import crestedbutte.NotificationStuff.{desiredAlarms, headsUpAmount}
import crestedbutte.*
import org.scalajs.dom
import org.scalajs.dom.NotificationOptions
import org.scalajs.dom.experimental.{
  Notification,
  NotificationOptions,
}
import org.scalajs.dom.raw.Position

import scala.scalajs.js

object Experimental {
  import com.raquo.laminar.api.L._

  def showPosition(
    position: Position,
  ) =
    println(
      "Latitude: " + position.coords.latitude + "  Longitude: " + position.coords.longitude,
    )

  def getLocation(
    $gpsPosition: Var[Option[GpsCoordinates]],
  ): Option[GpsCoordinates] = {
    var positionResult: Option[GpsCoordinates] = None

    if (dom.window.navigator.geolocation != null) {
      dom.window.navigator.geolocation.getCurrentPosition(
        successCallback = position => {
          showPosition(position)
          $gpsPosition.set(
            Some(
              GpsCoordinates(latitude = position.coords.latitude,
                             longitude = position.coords.longitude,
              ),
            ),
          )
          positionResult = Some(
            GpsCoordinates(latitude = position.coords.latitude,
                           longitude = position.coords.longitude,
            ),
          )
        },
      );
    }
    else {
      println("Geo Location not supported by browser");
    }
    positionResult
  }

  object Notifications {

    val clickObserver = Observer[dom.MouseEvent](
      onNext = ev =>
        if (dom.Notification.permission == "default")
          dom.Notification.requestPermission(response =>
            println(
              "Notification requestPermission response: " + response,
            ),
          )
        else if (dom.Notification.permission == "denied")
          println(
            "They denied permission to notifications. Give it up.",
          )
        else if (dom.Notification.permission == "granted")
          println("we already have permission.")
        else
          println(
            "Uknown permission state: " + dom.Notification.permission,
          ),
    )

  }

  def Sandbox(
    timeStamps: Signal[WallTime],
  ) = {

    val $gpsPosition: Var[Option[GpsCoordinates]] = Var(None)
    div(
      idAttr := "sandbox",
      timeStamps.map { _ =>
        println("current location: " + getLocation($gpsPosition))
        getLocation($gpsPosition)
      } --> $gpsPosition.writer,
//      Components.FeatureControlCenter(featureUpdates.writer),
      button(
        idAttr := "Get position",
        onClick --> Observer[dom.MouseEvent](
          onNext = ev => getLocation($gpsPosition),
        ),
        "Get GPS coords",
      ),
      if (dom.Notification.permission == "granted")
        span("Notifications are enabled")
      else
        button(
          idAttr := ElementNames.Notifications.requestPermission,
          cls := "button",
          "Enable Notifications",
          onClick --> Notifications.clickObserver,
        ),
    )
  }

  def GeoBits(
    $mapLinksEnabled: Signal[Boolean],
    location: Location,
    $gpsPosition: Signal[Option[GpsCoordinates]],
  ) = {
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

    div(
      child <-- $mapLinksEnabled.map(mapLinksEnabled =>
        if (mapLinksEnabled)
          div(
            cls := "map-link",
            child <--
              distanceFromCurrentLocationToStop($gpsPosition,
                                                location,
              ),
            location.gpsCoordinates.map(GeoLink),
          )
        else
          div(),
      ),
    )
  }

  def GPS(
    gpsPosition: Var[Option[GpsCoordinates]],
  ) =
    button(
      idAttr := "Get position",
      onClick --> Observer[dom.MouseEvent]: ev =>
        getLocation(gpsPosition),
      "Get GPS coords",
    )

  def FeatureControlCenter(
    featureUpdates: WriteBus[FeatureStatus],
  ) = {

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
      SvgIcon.map(),
    )

}
