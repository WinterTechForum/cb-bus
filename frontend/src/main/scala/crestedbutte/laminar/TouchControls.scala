package crestedbutte.laminar

import com.raquo.laminar.api.L.*
import com.raquo.laminar.nodes.ReactiveElement
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

  /** Direction-locked swipe helper for the common "swipe to reveal
    * delete" UX.
    *   - Locks gesture to horizontal vs vertical based on initial
    *     movement.
    *   - When horizontal, updates `offsetPx` and blocks vertical
    *     wheel scrolling.
    *   - When vertical, re-enables wheel scrolling and ignores
    *     horizontal logic. Returns a Modifier to attach to the
    *     interactive element, and a Var that can be passed to
    *     components like the scrolling wheel to allow vertical
    *     dragging.
    */
  def swipeToRevealWithDelete(
    deleteRevealWidthPx: Double,
    isRevealed: Var[Boolean],
    offsetPx: Var[Double],
    onDelete: () => Unit,
    revealTriggerDeltaPx: Double = 40.0,
    revealThresholdRatio: Double = 0.4,
    directionThresholdPx: Double = 8.0,
  ): (Modifier[ReactiveElement.Base], Var[Boolean]) = {
    val touchStartX: Var[Double] = Var(0.0)
    val touchStartY: Var[Double] = Var(0.0)
    val baseOffsetAtStart: Var[Double] = Var(0.0)
    val gestureLock: Var[Option[String]] = Var(
      None,
    ) // "horizontal" | "vertical"
    val allowVerticalDrag: Var[Boolean] = Var(true)

    val modifier = Modifier { el =>
      el.amend(
        onTouchStart --> Observer { (e: dom.TouchEvent) =>
          gestureLock.set(None)
          // Disable wheel until we know it's a vertical gesture
          allowVerticalDrag.set(false)
          val t = e.touches(0)
          val x = t.clientX
          val y = t.clientY
          touchStartX.set(x)
          touchStartY.set(y)
          baseOffsetAtStart.set(
            if (isRevealed.now()) deleteRevealWidthPx else 0.0,
          )
        },
        onTouchMove --> Observer { (e: dom.TouchEvent) =>
          val t = e.touches(0)
          val currentX = t.clientX
          val currentY = t.clientY
          val dx = Math.abs(touchStartX.now() - currentX)
          val dy = Math.abs(touchStartY.now() - currentY)

          if (gestureLock.now().isEmpty) {
            if (dx > directionThresholdPx && dx > dy) {
              gestureLock.set(Some("horizontal"))
              allowVerticalDrag.set(false)
            }
            else if (dy > directionThresholdPx && dy > dx) {
              gestureLock.set(Some("vertical"))
              allowVerticalDrag.set(true)
            }
          }

          gestureLock.now() match {
            case Some("horizontal") =>
              e.preventDefault()
              e.stopPropagation()
              val delta =
                touchStartX.now() - currentX // left is positive
              val proposed =
                Math.max(
                  0.0,
                  Math.min(
                    deleteRevealWidthPx,
                    baseOffsetAtStart.now() + delta,
                  ),
                )
              offsetPx.set(proposed)
            case Some("vertical") =>
              () // let vertical consumer handle it
            case _ =>
              ()
          }
        },
        onTouchEnd --> Observer { (e: dom.TouchEvent) =>
          gestureLock.now() match {
            case Some("horizontal") =>
              e.preventDefault()
              e.stopPropagation()
              val endX = e.changedTouches(0).clientX
              val delta = touchStartX.now() - endX
              if (isRevealed.now() && delta > revealTriggerDeltaPx) {
                onDelete()
              }
              else {
                val shouldReveal =
                  offsetPx
                    .now() > (deleteRevealWidthPx * revealThresholdRatio)
                isRevealed.set(shouldReveal)
                offsetPx.set(
                  if (shouldReveal) deleteRevealWidthPx else 0.0,
                )
              }
            case _ =>
              ()
          }
          gestureLock.set(None)
          allowVerticalDrag.set(true)
        },
      )
    }

    (modifier, allowVerticalDrag)
  }

  /** Threshold-based swipe-to-delete interaction without revealing a
    * button. While swiping horizontally, `offsetPx` is updated to
    * reflect the live translation. On release, if the accumulated
    * offset exceeds the computed trigger threshold, the provided
    * `onDelete` is invoked. Otherwise, the item snaps back to its
    * original position (offset = 0).
    *
    * The trigger threshold is calculated as the maximum of
    * `minTriggerPx` and `deleteTriggerRatio * elementWidth` to scale
    * with larger items while remaining usable on small ones.
    */
  def swipeToDelete(
    deleteTriggerRatio: Double = 0.35,
    minTriggerPx: Double = 100.0,
    offsetPx: Var[Double],
    onDelete: () => Unit,
    directionThresholdPx: Double = 8.0,
  ): (Modifier[ReactiveElement.Base], Var[Boolean]) = {
    val touchStartX: Var[Double] = Var(0.0)
    val touchStartY: Var[Double] = Var(0.0)
    val baseOffsetAtStart: Var[Double] = Var(0.0)
    val gestureLock: Var[Option[String]] = Var(
      None,
    ) // "horizontal" | "vertical"
    val allowVerticalDrag: Var[Boolean] = Var(true)

    val modifier = Modifier { el =>
      el.amend(
        onTouchStart --> Observer { (e: dom.TouchEvent) =>
          gestureLock.set(None)
          allowVerticalDrag.set(false)
          val t = e.touches(0)
          touchStartX.set(t.clientX)
          touchStartY.set(t.clientY)
          baseOffsetAtStart.set(0.0)
        },
        onTouchMove --> Observer { (e: dom.TouchEvent) =>
          val t = e.touches(0)
          val currentX = t.clientX
          val currentY = t.clientY
          val dx = Math.abs(touchStartX.now() - currentX)
          val dy = Math.abs(touchStartY.now() - currentY)

          if (gestureLock.now().isEmpty) {
            if (dx > directionThresholdPx && dx > dy) {
              gestureLock.set(Some("horizontal"))
              allowVerticalDrag.set(false)
            }
            else if (dy > directionThresholdPx && dy > dx) {
              gestureLock.set(Some("vertical"))
              allowVerticalDrag.set(true)
            }
          }

          gestureLock.now() match {
            case Some("horizontal") =>
              e.preventDefault()
              e.stopPropagation()
              val delta =
                touchStartX.now() - currentX // left is positive
              val proposed = baseOffsetAtStart.now() + delta
              // Clamp symmetrically to element width in both directions
              val width =
                el.ref.asInstanceOf[dom.Element].clientWidth.toDouble
              val clamped =
                if (width > 0)
                  Math.max(-width, Math.min(proposed, width))
                else proposed
              offsetPx.set(clamped)
            case Some("vertical") =>
              ()
            case _ =>
              ()
          }
        },
        onTouchEnd --> Observer { (e: dom.TouchEvent) =>
          gestureLock.now() match {
            case Some("horizontal") =>
              e.preventDefault()
              e.stopPropagation()
              val width =
                el.ref.asInstanceOf[dom.Element].clientWidth.toDouble
              val trigger =
                Math.max(minTriggerPx, width * deleteTriggerRatio)
              val endX = e.changedTouches(0).clientX
              val totalDelta =
                touchStartX
                  .now() - endX // left positive, right negative
              val absDelta = Math.abs(totalDelta)
              if (absDelta >= trigger) {
                onDelete()
              }
              else {
                offsetPx.set(0.0)
              }
            case _ =>
              ()
          }
          gestureLock.set(None)
          allowVerticalDrag.set(true)
        },
      )
    }

    (modifier, allowVerticalDrag)
  }

}
