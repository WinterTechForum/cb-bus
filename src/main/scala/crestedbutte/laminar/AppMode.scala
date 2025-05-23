package crestedbutte.laminar

enum AppMode:
  case Production, dev, Premium

object AppMode {
  import upickle.default._
  implicit val PremiumRW: ReadWriter[Premium.type] = macroRW
  implicit val ProductionRW: ReadWriter[Production.type] = macroRW
  implicit val devRW: ReadWriter[dev.type] = macroRW
  implicit val AppModeRW: ReadWriter[AppMode] = macroRW

}
