package todo.facades

import scala.scalajs.js
import scala.scalajs.js.annotation._
import org.scalajs.dom.experimental.serviceworkers.WindowClient

// Extension to check if WindowClient supports navigation
object WindowClientExtensions {
  implicit class WindowClientOps(val client: WindowClient) extends AnyVal {
    def navigateOption: Option[String => js.Promise[WindowClient]] = {
      val dyn = client.asInstanceOf[js.Dynamic]
      if (!js.isUndefined(dyn.navigate) && dyn.navigate != null) {
        Some((url: String) => dyn.navigate(url).asInstanceOf[js.Promise[WindowClient]])
      } else {
        None
      }
    }
    
    def canNavigate: Boolean = navigateOption.isDefined
  }
}