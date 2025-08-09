package crestedbutte.laminar

import com.raquo.laminar.api.L.*
import crestedbutte.*
import org.scalajs.dom
import org.scalajs.dom.TouchEvent
import scala.scalajs.js

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
    touchBus.events.flatMapSwitch(e =>
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
  val touchstartTimeMs: Var[Double] = Var(0)

  def swipeProp(
    swipeEvent: Swipe => Unit,
  ) =
    Modifier { el =>
      el.amend(
        onTouchStart.map(_.changedTouches(0).screenX) --> touchstartX,
        onTouchStart.map(_ => js.Date.now()) --> touchstartTimeMs,
        onTouchEnd.flatMap { t =>
          val start = touchstartX.now()
          val end = t.changedTouches(0).screenX
          val significantSwipe = Math.abs(start - end) > 50
          val opt =
            if (significantSwipe)
              if (start > end)
                Some(Swipe.Left)
              else
                Some(Swipe.Right)
            else
              None
          EventStream.fromSeq(
            opt.toSeq,
          )
        } --> Observer {
          swipeEvent
        },
      )
    }

  /** Momentum-based swipe that maps a single swipe into N steps
    * (positive for left/next, negative for right/previous). Steps are
    * derived from distance and velocity of the swipe.
    */
  def swipeThrowProp(
    onThrow: Int => Unit,
    // tuning knobs (in pixels/ms and pixels per step)
    pixelsPerStep: Double = 80.0,
    velocityGain: Double = 200.0,
    maxSteps: Int = 6,
  ) =
    Modifier { el =>
      el.amend(
        onTouchStart.map(_.changedTouches(0).screenX) --> touchstartX,
        onTouchStart.map(_ => js.Date.now()) --> touchstartTimeMs,
        onTouchEnd.flatMap { t =>
          val startX = touchstartX.now()
          val endX = t.changedTouches(0).screenX
          val deltaX = startX - endX // positive means leftwards
          val magnitude = Math.abs(deltaX)
          val elapsedMs =
            Math.max(1.0, js.Date.now() - touchstartTimeMs.now())
          val pxPerMs = magnitude / elapsedMs

          val base = magnitude / pixelsPerStep
          val boost = pxPerMs * (velocityGain / pixelsPerStep)
          val rawSteps = Math.round(base + boost).toInt
          val clampedSteps = Math.max(0, Math.min(maxSteps, rawSteps))

          val signedSteps =
            if (clampedSteps == 0) 0
            else if (deltaX > 0) clampedSteps // left → next
            else -clampedSteps // right → previous

          EventStream.fromSeq[Int](
            Option.when(signedSteps != 0)(signedSteps).toSeq,
          )
        } --> Observer[Int] { n =>
          onThrow(n)
        },
      )
    }

}
