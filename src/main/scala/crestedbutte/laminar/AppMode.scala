package crestedbutte.laminar

import enumeratum._

sealed trait AppMode extends EnumEntry

object AppMode extends Enum[AppMode] {
  val values = findValues // macro

  case object Production extends AppMode
  case object dev extends AppMode
  case object Premium extends AppMode

}
