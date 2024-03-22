package crestedbutte

import com.billding.time.WallTime

import zio.json._

case class LocationWithTime(
                             l: Location,
                             t: WallTime)
  derives JsonCodec
