package crestedbutte.sandbox

import crestedbutte.DomManipulation
import zio.*

object ScheduleSandbox {
  import scalatags.JsDom.all._

  val driveFromMountaineerToTeocalli =
    Schedule.once && Schedule.spaced(2.seconds)

  def realBusSchedule(
    numberOfBussesPerDay: Int,
  ) =
    Schedule.recurs(numberOfBussesPerDay) &&
      Schedule.spaced(2.seconds)

  def singleBusRoute(
    number: Int,
  ) =
    DomManipulation
      .appendMessageToPage(
        div(s"Bus #$number is leaving old town hall!").render,
      )
      .flatMap(_ => ZIO.sleep(4.seconds))
      .flatMap(_ =>
        DomManipulation.appendMessageToPage(
          div(s"Bus #$number arrived at clarks!").render,
        ),
      )
      .flatMap(_ => ZIO.sleep(1.seconds))
      .flatMap(_ =>
        DomManipulation
          .appendMessageToPage(
            div(s"Bus #$number arrived at 4 way!").render,
          ),
      )
      .flatMap(_ => ZIO.sleep(1.seconds))
      .flatMap(_ =>
        DomManipulation
          .appendMessageToPage(
            div(s"Bus #$number arrived at Teocalli!").render,
          ),
      )
      .flatMap(_ => ZIO.sleep(5.seconds)) // TODO Proper amount here
      .flatMap(_ =>
        DomManipulation.appendMessageToPage(
          div(s"Bus #$number arrived at Mountaineer Square!").render,
        ),
      )

  val secondBus =
    ZIO
      .sleep(4.seconds)
      .flatMap(_ => singleBusRoute(2))

  val thirdBus =
    ZIO
      .sleep(8.seconds)
      .flatMap(_ => singleBusRoute(3))

  val liveBusses =
    ZIO.reduceAllPar(singleBusRoute(1), List(secondBus, thirdBus)) {
      case (_, _) => "bah"
    }

//      .repeat(realBusSchedule(5))
}
