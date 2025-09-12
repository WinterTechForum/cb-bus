package crestedbutte

import com.billding.time.WallTime
import com.raquo.laminar.api.L.*
import org.scalajs.dom
import org.scalajs.dom.{Notification, NotificationOptions}
import scala.scalajs.js
import scala.scalajs.js.timers.*
import scala.concurrent.duration.*

object NotificationCountdown {
  private val NOTIFICATION_TAG = "route-countdown"
  private var updateInterval: Option[SetIntervalHandle] = None
  
  def requestPermissionIfNeeded(): Unit = {
    if (dom.Notification.permission == "default") {
      dom.Notification.requestPermission { response =>
        println(s"Notification permission response: $response")
      }
    }
  }
  
  def hasPermission: Boolean = dom.Notification.permission == "granted"
  
  def startCountdownNotifications($plan: Signal[Plan], timeStamps: Signal[WallTime]): Unit = {
    // Clear any existing interval
    stopCountdownNotifications()
    
    // Update immediately
    updateNotification($plan.now(), timeStamps.now())
    
    // Then update every 30 seconds
    updateInterval = Some(setInterval(30.seconds) {
      updateNotification($plan.now(), timeStamps.now())
    })
  }
  
  def stopCountdownNotifications(): Unit = {
    updateInterval.foreach(clearInterval)
    updateInterval = None
    clearNotification()
  }
  
  private def updateNotification(plan: Plan, currentTime: WallTime): Unit = {
    findNextSegmentTime(plan, currentTime) match {
      case Some((segmentTime, routeName)) =>
        val minutesUntil = segmentTime.between(currentTime).toMinutes
        showNotification(minutesUntil, routeName)
      case None =>
        clearNotification()
    }
  }
  
  private def findNextSegmentTime(plan: Plan, currentTime: WallTime): Option[(WallTime, String)] = {
    plan.routeSegments
      .map(segment => (segment.s.t, segment.routeWithTimes.componentName))
      .find { case (segmentTime, _) => segmentTime.isAfter(currentTime) }
  }
  
  private def showNotification(minutesUntil: Long, routeName: String): Unit = {
    if (hasPermission) {
      val message = if (minutesUntil <= 0) {
        s"$routeName route starting now!"
      } else if (minutesUntil == 1) {
        s"$routeName route starts in 1 minute"
      } else {
        s"$routeName route starts in $minutesUntil minutes"
      }
      
      new Notification(
        message,
        NotificationOptions(
          tag = NOTIFICATION_TAG,
          requireInteraction = false,
          silent = true,
          renotify = false
        )
      )
    }
  }
  
  private def clearNotification(): Unit = {
    // There's no direct way to clear a notification in the web API,
    // but creating a new one with the same tag will replace the old one
    if (hasPermission) {
      val notification = new Notification(
        "",
        NotificationOptions(
          tag = NOTIFICATION_TAG,
          silent = true
        )
      )
      // Close it immediately
      js.timers.setTimeout(0) {
        notification.close()
      }
    }
  }
}