package crestedbutte

import com.billding.time.BusTime
import crestedbutte.Browser.Browser
import org.scalajs.dom.MouseEvent
import zio.ZIO

import scala.collection.mutable

object NotificationStuff {
  val desiredAlarms = mutable.Queue.empty[BusTime]
  desiredAlarms.empty
}
