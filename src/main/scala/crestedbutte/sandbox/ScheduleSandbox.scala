package crestedbutte.sandbox

import crestedbutte.dom.DomManipulation
import zio.clock.Clock
import zio.{duration, Schedule, ZIO}

object ScheduleSandbox {
  import scalatags.JsDom.all._

  val driveFromMountaineerToTeocalli =
    Schedule.once && Schedule.spaced(duration.durationInt(2).seconds)

  def realBusSchedule(
    numberOfBussesPerDay: Int,
  ): Schedule[Clock, Any, (Int, Int)] =
    Schedule.recurs(numberOfBussesPerDay) &&
    Schedule.spaced(duration.durationInt(2).seconds)

  def singleBusRoute(
    number: Int,
  ) =
    DomManipulation
      .appendMessageToPage(
        div(s"Bus #$number is leaving old town hall!").render,
      )
      .flatMap(_ => ZIO.sleep(duration.durationInt(4).seconds))
      .flatMap(
        _ =>
          DomManipulation.appendMessageToPage(
            div(s"Bus #$number arrived at clarks!").render,
          ),
      )
      .flatMap(_ => ZIO.sleep(duration.durationInt(1).seconds))
      .flatMap(
        _ =>
          DomManipulation
            .appendMessageToPage(
              div(s"Bus #$number arrived at 4 way!").render,
            ),
      )
      .flatMap(_ => ZIO.sleep(duration.durationInt(1).seconds))
      .flatMap(
        _ =>
          DomManipulation
            .appendMessageToPage(
              div(s"Bus #$number arrived at Teocalli!").render,
            ),
      )
      .flatMap(_ => ZIO.sleep(duration.durationInt(5).seconds)) // TODO Proper amount here
      .flatMap(
        _ =>
          DomManipulation.appendMessageToPage(
            div(s"Bus #$number arrived at Mountaineer Square!").render,
          ),
      )

  val secondBus =
    ZIO
      .sleep(duration.durationInt(4).seconds)
      .flatMap(_ => singleBusRoute(2))

  val thirdBus =
    ZIO
      .sleep(duration.durationInt(8).seconds)
      .flatMap(_ => singleBusRoute(3))

  val liveBusses =
    ZIO.reduceAllPar(singleBusRoute(1), List(secondBus, thirdBus)) {
      case (_, _) => "bah"
    }

//      .repeat(realBusSchedule(5))
}
