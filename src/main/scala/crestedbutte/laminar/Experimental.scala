package crestedbutte.laminar

import com.billding.time.{TimePicker, WallTime}
import crestedbutte.NotificationStuff.{desiredAlarms, headsUpAmount}
import crestedbutte.{
  ElementNames,
  FeatureStatus,
  GpsCoordinates,
  NotificationStuff,
}
import org.scalajs.dom
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

    def createJankyBusAlertInSideEffectyWay(
      busTime: WallTime,
      localTime: WallTime,
    ) =
      if (localTime
            .between(busTime)
            // TODO Direct comparison
            .toMinutes >= headsUpAmount.toMinutes)
        dom.window.setTimeout(
          // TODO Replace this with submission to an EventBus[WallTime] that can be read via the RepeatingElement
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

    def AlarmIcon(
      name: String,
      classes: String,
      busTime: WallTime,
    ) = {
      val clickObserverNarrow = Observer[WallTime](
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
    $gpsPosition: Var[Option[GpsCoordinates]],
    featureUpdates: EventBus[FeatureStatus],
  ) = {
    val newTimePicker = TimePicker("12:34")
    div(
      idAttr := "sandbox",
      timeStamps.map(
        _ => getLocation($gpsPosition),
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
      newTimePicker.component,
    )
  }

  sealed trait AM_OR_PM
  case object AM extends AM_OR_PM
  case object PM extends AM_OR_PM

  def Toggler(
    $value: Var[AM_OR_PM],
  ) = {
    val updates = new EventBus[Unit]
    val newNumberValues: EventStream[AM_OR_PM] =
      updates.events.withCurrentValueOf($value).map {
        case (curNumberValue) =>
          if (curNumberValue == AM)
            PM
          else
            AM

      }

    div(
      cls := "amOrPm wheel",
      button(
        cls := "arrival-time adjuster-button open-arrival-time-modal tp-inc",
        onClick.preventDefault.map(_ => ()) --> updates,
        img(
          cls := "glyphicon",
          src := "/glyphicons/svg/individual-svg/glyphicons-basic-222-chevron-up.svg",
        ),
      ),
      div(
        cls := "tp-display",
        child <-- $value.signal.map(_.toString),
      ),
      button(
        cls := "arrival-time adjuster-button open-arrival-time-modal tp-dec",
        onClick.preventDefault.map(_ => ()) --> updates,
        img(
          cls := "glyphicon",
          src := "/glyphicons/svg/individual-svg/glyphicons-basic-221-chevron-down.svg",
        ),
      ),
      newNumberValues --> $value,
    )

  }

  def NumberPicker(
    $number: Var[Int],
    deltaValue: Int,
    minValue: Int,
    maxValue: Int,
    sectionName: String,
  ) = {
    val updates = new EventBus[Int]
    val newNumberValues: EventStream[Int] =
      updates.events.withCurrentValueOf($number).map {
        case (delta, curNumberValue) =>
          if (delta > 0)
            if (curNumberValue + delta <= maxValue)
              curNumberValue + delta
            else
              curNumberValue
          else if (curNumberValue + delta >= minValue)
            curNumberValue + delta
          else
            curNumberValue

      }

    div(
      cls := s"$sectionName wheel",
      button(
        cls := s"arrival-time adjuster-button open-arrival-time-modal tp-inc",
        onClick.preventDefault.map(_ => deltaValue) --> updates,
        img(
          cls := "glyphicon",
          src := "/glyphicons/svg/individual-svg/glyphicons-basic-222-chevron-up.svg",
        ),
      ),
      div(
        cls := s"tp-display",
        child <-- $number.signal.map(_.toString),
      ),
      button(
        cls := s"arrival-time adjuster-button open-arrival-time-modal tp-dec",
        onClick.preventDefault.map(_ => -deltaValue) --> updates,
        img(
          cls := "glyphicon",
          src := "/glyphicons/svg/individual-svg/glyphicons-basic-221-chevron-down.svg",
        ),
      ),
      newNumberValues --> $number,
    )

  }
}
