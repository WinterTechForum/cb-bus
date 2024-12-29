package crestedbutte.laminar

import com.billding.time.{WallTime}
import crestedbutte.NotificationStuff.{desiredAlarms, headsUpAmount}
import crestedbutte.routes.RtaSouthbound
import crestedbutte.*
import org.scalajs.dom
import org.scalajs.dom.NotificationOptions
import org.scalajs.dom.experimental.{
  Notification,
  NotificationOptions,
}
import org.scalajs.dom.raw.Position
import typings.std.global.navigator

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

    def createJankyBusAlertInSideEffectyWay(
      busTime: WallTime,
      localTime: WallTime,
    ) = {
      println("Dequeued time for alarm: " + busTime)
//      if (
//        localTime
//          .between(busTime)
//          // TODO Direct comparison
//          .toMinutes <= headsUpAmount.toMinutes
//      ) {
//        println("Setting timeout")
//        dom.window.setTimeout(
      // TODO Replace this with submission to an EventBus[WallTime] that can be read via the RepeatingElement
//          () =>
      // Read submitted time, find difference between it and the current time, then submit a setInterval function
      // with the appropriate delay
      new Notification(
        s"The ${busTime.toString} bus is arriving in ${headsUpAmount.toMinutes} minutes!",
        NotificationOptions(
          vibrate = js.Array(100d),
        ),
      )
//          0,
//          (localTime
//            .between(busTime)
//            .toMinutes - headsUpAmount.toMinutes) * 60 * 1000,
//        )
//      }
    }

    def AlarmIcon(
      name: String,
      classes: String,
      busTime: WallTime,
    ) = {
      val clickObserverNarrow = Observer[WallTime](
        onNext = ev => {
          println("Clicked alarm")
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
      Components
        .SvgIcon(name)
        .amend(
          verticalAlign := "middle",
          onClick.map(_ => busTime) --> clickObserverNarrow,
        )
    }
  }

  def Sandbox(
    timeStamps: Signal[WallTime],
  ) = {

    val $gpsPosition: Var[Option[GpsCoordinates]] = Var(None)
//    val featureUpdates = new EventBus[FeatureStatus]
    div(
      idAttr := "sandbox",
      timeStamps.map(_ => getLocation($gpsPosition),
      ) --> $gpsPosition.writer,
//      Components.FeatureControlCenter(featureUpdates.writer),
      button(
        idAttr := "Get position",
        onClick --> Observer[dom.MouseEvent](
          onNext = ev => getLocation($gpsPosition),
        ),
        "Get GPS coords",
      ),
      button(
        idAttr := ElementNames.Notifications.requestPermission,
        cls := "button",
        "Request Notifications Permission",
        onClick --> Notifications.clickObserver,
      ),
    )
  }

  def manualClunkyAlerts(
    $alertsEnabled: Signal[Boolean],
    time: WallTime,
  ) =
    div(
      child <-- $alertsEnabled.map(alertsEnabled =>
        if (dom.Notification.permission == "granted" && alertsEnabled)
          Experimental.Notifications.AlarmIcon(
            "glyphicons-basic-443-bell-ringing.svg",
            "arrival-time-alarm",
            time,
          )
        else div(),
      ),
    )

}
