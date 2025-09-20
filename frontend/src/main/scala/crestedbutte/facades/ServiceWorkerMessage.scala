package crestedbutte.facades

import scala.scalajs.js
import scala.scalajs.js.annotation._

// Typed facade for ServiceWorker message data
@js.native
trait ServiceWorkerMessageData extends js.Object {
  val kind: js.UndefOr[String] = js.native
  val message: js.UndefOr[String] = js.native
  val status: js.UndefOr[String] = js.native
}

object ServiceWorkerMessageData {
  def apply(
    kind: js.UndefOr[String] = js.undefined,
    message: js.UndefOr[String] = js.undefined,
    status: js.UndefOr[String] = js.undefined
  ): ServiceWorkerMessageData = {
    js.Dynamic.literal(
      kind = kind,
      message = message,
      status = status
    ).asInstanceOf[ServiceWorkerMessageData]
  }
  
  def unapply(data: js.Any): Option[ServiceWorkerMessageData] = {
    try {
      Some(data.asInstanceOf[ServiceWorkerMessageData])
    } catch {
      case _: Exception => None
    }
  }
}