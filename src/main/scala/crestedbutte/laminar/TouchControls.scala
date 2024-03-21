package crestedbutte.laminar

import com.raquo.laminar.api.L.*
import crestedbutte.*
import crestedbutte.routes.{RtaNorthbound, RtaSouthbound}
import org.scalajs.dom
import crestedbutte.laminar.Experimental.getLocation
import crestedbutte.pwa.Persistence
import com.billding.time.{MinuteDuration, WallTime}
import com.raquo.laminar.nodes.ReactiveHtmlElement
import crestedbutte.NotificationStuff.desiredAlarms
import crestedbutte.*
import crestedbutte.dom.BulmaLocal
import crestedbutte.routes.{AllRoutes, RtaSouthbound, SpringFallLoop, TownShuttleTimes}
import org.scalajs.dom

import java.time.format.{DateTimeFormatter, FormatStyle}
import java.time.{Clock, OffsetDateTime}
import scala.concurrent.duration.FiniteDuration
import crestedbutte.dom.BulmaLocal.ModalMode
import org.scalajs.dom.{TouchEvent, document}

import scala.collection.immutable.{AbstractSeq, LinearSeq}
import scala.util.Random

object TouchControls {
  val onTouchMove: EventProp[dom.TouchEvent] = eventProp("touchmove")

  val onTouchStart: EventProp[dom.TouchEvent] =
    eventProp("touchstart")

  val onTouchEnd: EventProp[dom.TouchEvent] =
    eventProp("touchend")

  enum Swipe:
    case Left, Right

  val touchBus = EventBus[dom.TouchEvent]()

  val swipeEvents =
    touchBus.events.flatMap( e =>
      EventStream.fromSeq(
        Option.when(Random.nextBoolean())(
          e
        ).toSeq
      )
    )


  val touchstartX: Var[Double] = Var(0)
  val touchendX: Var[Double] = Var(10)

//  val swipeEvent: EventProp[Swipe] =

  def swipeProp(swipeEvent: Swipe => Unit) =
    Modifier {
      el =>
        println("Amending modifiers!")
        el.amend(

          onTouchStart.map(_.changedTouches(0).screenX) --> touchstartX,
          onTouchEnd.flatMap{
            t =>
              val start = touchstartX.now()
              val end = t.changedTouches(0).screenX
              println("Start: " + start + "  End: " + end)
              val opt = if (start > end)
                Some(Swipe.Left)
              else if (end > start)
                Some(Swipe.Right)
              else
                None
              println("opt: " + opt)
              EventStream.fromSeq(
                (opt).toSeq
              )
          }--> Observer {
            swipeEvent
          }

        )
    }


//  def initialize() = {
//
//    def checkDirection() = {
//      if (touchendX.now() < touchstartX.now())
//        println("swiped left ! ")
//      if (touchendX.now() > touchstartX.now())
//        println("swiped right ! ")
//    }
//
//    document.addEventListener("touchstart", (e: TouchEvent) => {
//      touchstartX.set(e.changedTouches(0).screenX)
//    }
//    )
//
//    document.addEventListener("touchend", (e: TouchEvent) => {
//      touchendX.set(e.changedTouches(0).screenX)
//      checkDirection()
//    }
//    )
//
//
//  }
}
