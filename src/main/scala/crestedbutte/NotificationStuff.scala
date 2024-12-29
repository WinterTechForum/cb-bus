package crestedbutte

import com.billding.time.{MinuteDuration, WallTime}

import scala.collection.mutable

object NotificationStuff {
  val headsUpAmount = MinuteDuration.ofMinutes(3) // minutes
  val desiredAlarms = mutable.Queue.empty[WallTime]
  desiredAlarms.empty
}
