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

object TouchControls {
  def initialize() = {
    val touchstartX: Var[Double] = Var(0)
    val touchendX: Var[Double] = Var(10)

    def checkDirection() = {
      if (touchendX.now() < touchstartX.now())
        println("swiped left ! ")
      if (touchendX.now() > touchstartX.now())
        println("swiped right ! ")
    }

    document.addEventListener("touchstart", (e: TouchEvent) => {
      touchstartX.set(e.changedTouches(0).screenX)
    }
    )

    document.addEventListener("touchend", (e: TouchEvent) => {
      touchendX.set(e.changedTouches(0).screenX)
      checkDirection()
    }
    )


  }
}
