package crestedbutte.laminar

import com.raquo.laminar.api.L.*
import org.scalajs.dom

object OfflineStatus {
  val ready = Var(false)
  val problem = Var(Option.empty[String])
  val update = Var(Option.empty[() => Unit])

  def element(canUpdate: Signal[Boolean]): HtmlElement = {
    val online = Var(dom.window.navigator.onLine)
    div(cls := "offline-status", role := "status",
      windowEvents(_.onOnline) --> Observer { _ => online.set(true) },
      windowEvents(_.onOffline) --> Observer { _ => online.set(false) },
      span(child.text <-- ready.signal.combineWith(problem.signal).combineWith(online.signal).map {
        case (true, _, connected) =>
          if (connected) "Available offline" else "Offline · using downloaded schedule"
        case (false, Some(message), _) => message
        case (false, _, false) => "Offline setup incomplete. Connect once to download the app."
        case _ => "Preparing offline access…"
      }),
      child <-- update.signal.map {
        case Some(activate) => button(cls := "text-button", "Update app",
          disabled <-- canUpdate.map(!_),
          onClick --> Observer { _ => activate() })
        case None => emptyNode
      },
    )
  }
}
