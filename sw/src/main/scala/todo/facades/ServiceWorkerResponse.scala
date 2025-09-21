package todo.facades

import scala.scalajs.js
import scala.scalajs.js.annotation._

// Typed facade for ServiceWorker response data
@js.native
trait ServiceWorkerResponse extends js.Object {
  var status: String = js.native
}

object ServiceWorkerResponse {
  def apply(status: String): ServiceWorkerResponse = {
    js.Dynamic.literal(status = status).asInstanceOf[ServiceWorkerResponse]
  }
}