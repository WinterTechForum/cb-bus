package crestedbutte.dom

import animus.Animation
import com.raquo.laminar.api.L.*
import com.raquo.laminar.nodes.ReactiveHtmlElement
import crestedbutte.*
import crestedbutte.laminar.LocationTimeDirection

sealed trait StopContext
object StopContext {
  case object Departure extends StopContext
  case object Arrival extends StopContext
}
