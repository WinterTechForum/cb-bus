package crestedbutte

import enumeratum._

sealed trait Feature extends EnumEntry

object Feature extends Enum[Feature] {

  /*
   `findValues` is a protected method that invokes a macro to find all `Feature` object declarations inside an `Enum`

   You use it to implement the `val values` member
   */
  val values = findValues

  case object MapLinks extends Feature
  case object BusAlarms extends Feature
}
