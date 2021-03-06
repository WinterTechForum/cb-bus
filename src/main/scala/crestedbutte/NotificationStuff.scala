package crestedbutte

import com.billding.time.{BusDuration, BusTime}
import crestedbutte.Browser.Browser
import org.scalajs.dom.MouseEvent
import zio.ZIO

import scala.collection.mutable

object NotificationStuff {
  val headsUpAmount = BusDuration.ofMinutes(3) // minutes
  val desiredAlarms = mutable.Queue.empty[BusTime]
  desiredAlarms.empty
}
