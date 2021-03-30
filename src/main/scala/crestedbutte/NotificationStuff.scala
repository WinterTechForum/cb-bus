package crestedbutte

import com.billding.time.{MinuteDuration, WallTime}
import crestedbutte.Browser.Browser
import org.scalajs.dom.MouseEvent
import zio.ZIO

import scala.collection.mutable

object NotificationStuff {
  val headsUpAmount = MinuteDuration.ofMinutes(3) // minutes
  val desiredAlarms = mutable.Queue.empty[WallTime]
  desiredAlarms.empty
}
