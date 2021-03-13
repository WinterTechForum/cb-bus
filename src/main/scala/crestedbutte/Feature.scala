package crestedbutte

import enumeratum._

sealed trait Feature extends EnumEntry

object Feature extends Enum[Feature] {
  val values = findValues // macro

  case object MapLinks extends Feature
  case object BusAlarms extends Feature
}
