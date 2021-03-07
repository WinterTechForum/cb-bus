package crestedbutte.laminar

import com.billding.time.BusTime
import crestedbutte.{ElementNames, GpsCoordinates}
import org.scalajs.dom
import org.scalajs.dom.experimental.Notification
import org.scalajs.dom.raw.Position
import typings.std.global.navigator

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
    //    val permissionsLocal: permissions.PermissionsNavigator = org.scalajs.dom.experimental.permissions.toPermissions(navigator)
    //    permissionsLocal.permissions.query(PermissionDescriptor(org.scalajs.dom.experimental.permissions.PermissionName.geolocation))
    var positionResult: Option[GpsCoordinates] = None
    if (navigator.geolocation != null) {
      // TODO This callback is screwing me up. I think.
      navigator.geolocation.getCurrentPosition(
        successCallback = position => {
          showPosition(position)
          $gpsPosition.set(
            Some(
              GpsCoordinates(latitude = position.coords.latitude,
                             longitude = position.coords.longitude),
            ),
          )
          positionResult = Some(
            GpsCoordinates(latitude = position.coords.latitude,
                           longitude = position.coords.longitude),
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

  }

  def Sandbox(
    timeStamps: Signal[BusTime],
    $gpsPosition: Var[Option[GpsCoordinates]],
    featureUpdates: EventBus[FeatureStatus],
  ) =
    div(
      timeStamps.map(
        timestamp => getLocation($gpsPosition),
      ) --> $gpsPosition.writer,
      Components.FeatureControlCenter(featureUpdates.writer),
      button(
        idAttr := "Get position",
        onClick --> Observer[dom.MouseEvent](
          onNext = ev => {
            getLocation($gpsPosition)
          },
        ),
        "Get GPS coordinates",
      ),
      button(
        idAttr := ElementNames.Notifications.requestPermission,
        cls := "button",
        "Request Notifications Permission",
        onClick --> Notifications.clickObserver,
      ),
      button(
        idAttr := ElementNames.Notifications.submitMessageToServiceWorker,
        cls := "button",
        "SubmitMessage to SW",
      ),
    )

}
