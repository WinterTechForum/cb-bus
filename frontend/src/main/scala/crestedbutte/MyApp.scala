package crestedbutte

import crestedbutte.pwa.Persistence

import org.scalajs.dom
import crestedbutte.laminar.AppMode
import com.billding.time.WallTime
import crestedbutte.RTA

object MyApp extends App {
  ServiceWorkerClient
    .registerServiceWorker() // TODO Restore before pushing
  val appHolder = dom.document.getElementById("landing-message")
  appHolder.innerHTML = ""

  val persistence = Persistence()
  val saveSymbols = Map(
    "floppy"   -> "💾",
    "download" -> "⬇️",
    "bookmark" -> "🔖",
    "check"    -> "✅",
  )
  com.raquo.laminar.api.L.render(
    appHolder,
    RoutingStuff.app,
  )

  // Consume a shared link once. Reloads then recover the working draft instead
  // of re-importing the original itinerary over subsequent edits.
  val currentUrl = new dom.URL(dom.window.location.href)
  if (currentUrl.searchParams.has("plan")) {
    currentUrl.searchParams.delete("plan")
    dom.window.history.replaceState(dom.window.history.state, "", currentUrl.toString)
  }

}
