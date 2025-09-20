package todo.facades

import scala.scalajs.js
import scala.scalajs.js.annotation._
import org.scalajs.dom.raw.{MessageEvent, MessagePort}

// Typed facade for ExtendableMessageEvent with ports
@js.native
trait ExtendableMessageEvent extends MessageEvent {
  val ports: js.Array[MessagePort] = js.native
}