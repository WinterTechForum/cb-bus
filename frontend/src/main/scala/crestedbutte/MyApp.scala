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

}
