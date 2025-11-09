package crestedbutte.laminar

import animus.*
import com.billding.time.WallTime
import com.raquo.laminar.api.L.*
import com.raquo.laminar.nodes.ReactiveHtmlElement
import crestedbutte.*
import crestedbutte.NotificationStuff.desiredAlarms
import crestedbutte.dom.StopContext
import crestedbutte.laminar.TouchControls.Swipe
import crestedbutte.pwa.Persistence
import crestedbutte.{RTA, RouteWithTimes}
import crestedbutte.CompleteStopList
import java.time.format.DateTimeFormatter
import java.time.{Clock, OffsetDateTime}
import org.scalajs.dom
import org.scalajs.dom.{HTMLAnchorElement, HTMLDivElement}
import scala.scalajs.js
import scala.scalajs.js.timers._
import scala.concurrent.duration.FiniteDuration

object ScrollingWheel {
  def ScrollingWheel[T](
    items: Seq[T],
    itemToString: T => ReactiveHtmlElement[
      HTMLDivElement,
    ], // Should actually turn the item into a full element.
    initialIndex: Int = 0,
    initialSelectedElement: Option[T] = None,
    allowVerticalDrag: Var[Boolean] = Var(true),
  ): (ReactiveHtmlElement[HTMLDivElement], Signal[T]) = {
    val itemHeight = 60 // Height of each visible item in pixels
    val visibleItems = 3
    val containerHeight = itemHeight * visibleItems

    // Offset so the first item appears in the center (middle visible slot)
    val centerOffset = itemHeight

    // Calculate the actual initial index based on the provided element or fallback to initialIndex
    val actualInitialIndex = initialSelectedElement match {
      case Some(element) =>
        items.indexOf(element) match {
          case -1 =>
            initialIndex // If element not found, use initialIndex
          case index => index
        }
      case None => initialIndex
    }

    val selectedIndex: Var[Int] = Var(actualInitialIndex)
    val scrollPosition: Var[Double] = Var(
      (actualInitialIndex * itemHeight).toDouble,
    )
    val isDragging: Var[Boolean] = Var(false)
    val velocity: Var[Double] = Var(0.0)
    val lastTouchY: Var[Double] = Var(0.0)
    val lastTouchTime: Var[Double] = Var(0.0)

    // Animation loop for momentum scrolling
    var animationId: Option[Int] = None

    def startMomentumAnimation(): Unit = {
      animationId.foreach(dom.window.cancelAnimationFrame)

      def animate(): Unit =
        if (math.abs(velocity.now()) > 0.5 && !isDragging.now()) {
          val currentPos = scrollPosition.now()
          val newPos = currentPos + velocity.now()
          val maxScroll = (items.length - 1) * itemHeight

          val clampedPos = math.max(0, math.min(maxScroll, newPos))
          scrollPosition.set(clampedPos)

          // Apply friction
          velocity.update(_ * 0.95)

          // Snap to nearest item when velocity gets low
          if (math.abs(velocity.now()) < 2.0) {
            val targetIndex =
              math.round(clampedPos / itemHeight).toInt
            val clampedIndex =
              math.max(0, math.min(items.length - 1, targetIndex))
            val targetPos = clampedIndex * itemHeight
            scrollPosition.set(targetPos)
            selectedIndex.set(clampedIndex)
            velocity.set(0.0)
          }
          else {
            animationId = Some(
              dom.window.requestAnimationFrame(_ => animate()),
            )
          }
        }
        else {
          // Snap to nearest item
          val currentPos = scrollPosition.now()
          val targetIndex = math.round(currentPos / itemHeight).toInt
          val clampedIndex =
            math.max(0, math.min(items.length - 1, targetIndex))
          val targetPos = clampedIndex * itemHeight
          scrollPosition.set(targetPos)
          selectedIndex.set(clampedIndex)
        }

      // Always trigger the animation function, which will either start momentum or snap immediately
      animate()
    }

    val wheelElement = div(
      cls := "scrolling-wheel scrolling-wheel-container",
      styleProp("height") := s"${containerHeight}px",
      div(
        cls := "wheel-mask",

        // Center highlight only (no background gradients)
        div(
          styleAttr := s"position: absolute; top: ${itemHeight}px; left: 0; right: 0; height: ${itemHeight}px; border: 2px solid #019c8f; border-left: none; border-right: none; background: rgba(50, 115, 220, 0.1); pointer-events: none;",
        ),
      ),
      div(
        cls := "wheel-viewport",
        styleAttr := s"-webkit-mask-image: linear-gradient(to bottom, rgba(0,0,0,0) 0px, rgba(0,0,0,1) rgba(0,0,0,1) ${containerHeight - itemHeight}px, rgba(0,0,0,0) ${containerHeight}px); mask-image: linear-gradient(to bottom, rgba(0,0,0,0) 0px, rgba(0,0,0,1) ${itemHeight}px, rgba(0,0,0,1) ${containerHeight - itemHeight}px, rgba(0,0,0,0) ${containerHeight}px);",
        div(
          cls := "wheel-items",
          styleAttr <-- scrollPosition.signal.map(pos =>
            s"transform: translateY(${centerOffset - pos}px); transition: ${if (isDragging.now()) "none" else "transform 0.2s ease-out"};",
          ),

          // Actual items
          items.zipWithIndex.map { case (item, index) =>
            div(
              cls := "wheel-item",
              styleProp("height") := s"${itemHeight}px",
              itemToString(item),
            )
          },
        ),
      ),

      // Touch/mouse event handlers
      onMouseDown --> Observer { (e: dom.MouseEvent) =>
        isDragging.set(true)
        lastTouchY.set(e.clientY)
        lastTouchTime.set(dom.window.performance.now())
        velocity.set(0.0)
        animationId.foreach(dom.window.cancelAnimationFrame)
      },
      onMouseMove --> Observer { (e: dom.MouseEvent) =>
        if (isDragging.now()) {
          val currentY = e.clientY
          val deltaY = lastTouchY.now() - currentY
          val currentTime = dom.window.performance.now()
          val deltaTime = currentTime - lastTouchTime.now()

          if (deltaTime > 0) {
            velocity.set(deltaY / deltaTime * 16) // Scale for 60fps
          }

          val newPos = scrollPosition.now() + deltaY
          val maxScroll = (items.length - 1) * itemHeight
          val clampedPos = math.max(0, math.min(maxScroll, newPos))

          scrollPosition.set(clampedPos)
          lastTouchY.set(currentY)
          lastTouchTime.set(currentTime)
        }
      },
      onMouseUp --> Observer { (_: dom.MouseEvent) =>
        isDragging.set(false)
        startMomentumAnimation()
      },
      onMouseLeave --> Observer { (_: dom.MouseEvent) =>
        isDragging.set(false)
        startMomentumAnimation()
      },

      // Touch events for mobile
      TouchControls.onTouchStart --> Observer { (e: dom.TouchEvent) =>
        if (allowVerticalDrag.now()) {
          e.preventDefault()
          isDragging.set(true)
          val touch = e.touches(0)
          lastTouchY.set(touch.clientY)
          lastTouchTime.set(dom.window.performance.now())
          velocity.set(0.0)
          animationId.foreach(dom.window.cancelAnimationFrame)
        }
      },
      TouchControls.onTouchMove --> Observer { (e: dom.TouchEvent) =>
        if (allowVerticalDrag.now() && isDragging.now()) {
          e.preventDefault()
          val touch = e.touches(0)
          val currentY = touch.clientY
          val deltaY = lastTouchY.now() - currentY
          val currentTime = dom.window.performance.now()
          val deltaTime = currentTime - lastTouchTime.now()

          if (deltaTime > 0) {
            velocity.set(deltaY / deltaTime * 16)
          }

          val newPos = scrollPosition.now() + deltaY
          val maxScroll = (items.length - 1) * itemHeight
          val clampedPos = math.max(0, math.min(maxScroll, newPos))

          scrollPosition.set(clampedPos)
          lastTouchY.set(currentY)
          lastTouchTime.set(currentTime)
        }
      },
      TouchControls.onTouchEnd --> Observer { (e: dom.TouchEvent) =>
        if (allowVerticalDrag.now()) {
          e.preventDefault()
          isDragging.set(false)
          startMomentumAnimation()
        }
      },
    )

    val selectedItem = selectedIndex.signal.map(items(_))

    (wheelElement, selectedItem)
  }

}
