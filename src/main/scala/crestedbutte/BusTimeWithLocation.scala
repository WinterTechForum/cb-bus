package crestedbutte

import com.billding.time.BusTime

case class BusTimeWithLocation(
  busTime: BusTime,
  location: Location.Value,
)
