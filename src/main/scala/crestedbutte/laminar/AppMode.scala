package crestedbutte.laminar

import enumeratum._

sealed trait AppMode extends EnumEntry

object AppMode extends Enum[AppMode] {
  val values = findValues // macro

  case object Production extends AppMode
  case object dev extends AppMode
  case object Premium extends AppMode

  import upickle.default._
  implicit val PremiumRW: ReadWriter[Premium.type] = macroRW
  implicit val ProductionRW: ReadWriter[Production.type] = macroRW
  implicit val devRW: ReadWriter[dev.type] = macroRW
  implicit val AppModeRW: ReadWriter[AppMode] = macroRW

}
