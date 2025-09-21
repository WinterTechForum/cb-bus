package crestedbutte.facades

import scala.scalajs.js
import scala.scalajs.js.annotation._
import org.scalajs.dom

// Typed facade for Web Share API
@js.native
trait ShareData extends js.Object {
  var title: js.UndefOr[String] = js.native
  var text: js.UndefOr[String] = js.native
  var url: js.UndefOr[String] = js.native
}

object ShareData {
  def apply(
    title: js.UndefOr[String] = js.undefined,
    text: js.UndefOr[String] = js.undefined,
    url: js.UndefOr[String] = js.undefined
  ): ShareData = {
    js.Dynamic.literal(
      title = title,
      text = text,
      url = url
    ).asInstanceOf[ShareData]
  }
}

@js.native
trait NavigatorShare extends js.Object {
  def share(data: ShareData): js.Promise[Unit] = js.native
}

// Extension methods to check for share support
object NavigatorShareExtensions {
  implicit class NavigatorOps(val navigator: dom.Navigator) extends AnyVal {
    def shareOption: Option[NavigatorShare] = {
      // Use js.typeOf to check for share method existence
      if (js.typeOf(navigator.asInstanceOf[js.Dynamic].share) != "undefined") {
        Some(navigator.asInstanceOf[NavigatorShare])
      } else {
        None
      }
    }
    
    def canShare: Boolean = shareOption.isDefined
  }
}