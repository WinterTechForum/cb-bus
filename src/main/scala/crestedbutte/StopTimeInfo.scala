package crestedbutte

import com.billding.time.WallTime
import com.billding.time.MinuteDuration

case class StopTimeInfo(
  time: WallTime,
  waitingDuration: MinuteDuration)
