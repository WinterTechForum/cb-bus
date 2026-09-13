package crestedbutte.laminar

import com.raquo.laminar.api.L.*
import org.scalajs.dom
import scala.scalajs.js

/** Native dialog provides focus containment, Escape/back dismissal and top-layer rendering. */
object ModalSheet {
  def apply(open: Var[Boolean], heading: String, content: => HtmlElement): HtmlElement = {
    val sheet = htmlTag("dialog")(
      cls := "modal-sheet",
      role := "dialog",
      aria.label := heading,
      onMountCallback { ctx =>
        ctx.thisNode.ref.asInstanceOf[js.Dynamic].showModal()
      },
      eventProp[dom.Event]("cancel") --> Observer { _ => open.set(false) },
      div(cls := "sheet-heading", h2(heading),
        button(cls := "text-button", "Close", onClick --> Observer { _ => open.set(false) })),
      content,
    )
    sheet
  }
}
