package crestedbutte.laminar

import com.raquo.laminar.api.L.*
import crestedbutte.*
import org.scalajs.dom
import org.scalajs.dom.TouchEvent

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
    touchBus.events.flatMap(e =>
      EventStream.fromSeq(
        Option
          .when(Random.nextBoolean())(
            e,
          )
          .toSeq,
      ),
    )

  val touchstartX: Var[Double] = Var(0)
  val touchendX: Var[Double] = Var(10)

//  val swipeEvent: EventProp[Swipe] =

  def swipeProp(
    swipeEvent: Swipe => Unit,
  ) =
    Modifier { el =>
      println("Amending modifiers!")
      el.amend(
        onTouchStart.map(_.changedTouches(0).screenX) --> touchstartX,
        onTouchEnd.flatMap { t =>
          val start = touchstartX.now()
          val end = t.changedTouches(0).screenX
          val significantSwipe = Math.abs(start - end) > 50
          println("Start: " + start + "  End: " + end)
          val opt =
            if (significantSwipe)
              if (start > end)
                Some(Swipe.Left)
              else
                Some(Swipe.Right)
            else
              None
          println("opt: " + opt)
          EventStream.fromSeq(
            opt.toSeq,
          )
        } --> Observer {
          swipeEvent
        },
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
