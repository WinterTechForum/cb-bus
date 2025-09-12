package crestedbutte

import com.billding.time.WallTime
import com.raquo.laminar.api.L._
import org.scalajs.dom
import org.scalajs.dom.experimental.{Notification, NotificationOptions}
import scala.scalajs.js
import scala.scalajs.js.timers._
import scala.concurrent.duration._

object NotificationCountdown {
  private var updateInterval: Option[SetIntervalHandle] = None
  private var lastNotification: Option[Notification] = None
  
  def requestPermissionIfNeeded(): Unit = {
    if (dom.Notification.permission == "default") {
      dom.Notification.requestPermission { response =>
        println(s"Notification permission response: $response")
      }
    }
  }
  
  def hasPermission: Boolean = dom.Notification.permission == "granted"
  
  def startCountdownNotifications($plan: Var[Plan], timeStampsSignal: Signal[WallTime]): Unit = {
    // Clear any existing interval
    stopCountdownNotifications()
    
    // Keep track of current time
    var currentTime: WallTime = null
    
    // Subscribe to time updates
    timeStampsSignal.foreach { time =>
      currentTime = time
    }(unsafeWindowOwner)
    
    // Function to update the notification
    def update(): Unit = {
      if (currentTime != null) {
        val plan = $plan.now()
        findNextSegmentTime(plan, currentTime) match {
          case Some((segmentTime, routeName)) =>
            // Calculate minutes until segment start
            val minutesUntil = if (segmentTime.isAfter(currentTime)) {
              val diff = segmentTime.localTime.value - currentTime.localTime.value
              if (diff > 0) diff else 0
            } else 0
            
            showNotification(minutesUntil.toLong, routeName)
          case None =>
            clearNotification()
        }
      }
    }
    
    // Update immediately
    update()
    
    // Then update every 30 seconds
    updateInterval = Some(setInterval(30.seconds) {
      update()
    })
  }
  
  def stopCountdownNotifications(): Unit = {
    updateInterval.foreach(clearInterval)
    updateInterval = None
    clearNotification()
  }
  
  private def findNextSegmentTime(plan: Plan, currentTime: WallTime): Option[(WallTime, String)] = {
    plan.routeSegments
      .map(segment => (segment.s.t, segment.route.userFriendlyName))
      .find { case (segmentTime, _) => segmentTime.isAfter(currentTime) }
  }
  
  private def showNotification(minutesUntil: Long, routeName: String): Unit = {
    if (hasPermission) {
      // Close any existing notification
      lastNotification.foreach(_.close())
      
      val message = if (minutesUntil <= 0) {
        s"$routeName route starting now!"
      } else if (minutesUntil == 1) {
        s"$routeName route starts in 1 minute"
      } else {
        s"$routeName route starts in $minutesUntil minutes"
      }
      
      val notification = new Notification(
        message,
        NotificationOptions(
          vibrate = js.Array(100d)
        )
      )
      
      lastNotification = Some(notification)
    }
  }
  
  private def clearNotification(): Unit = {
    lastNotification.foreach(_.close())
    lastNotification = None
  }
}