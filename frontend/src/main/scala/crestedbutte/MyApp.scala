package crestedbutte

import crestedbutte.laminar.AppMode

import org.scalajs.dom

object MyApp extends App {
  println("06-07-2025 04:19 PM")

  // ServiceWorkerClient.registerServiceWorker() // TODO Restore before pushing

  val appHolder = dom.document.getElementById("landing-message")
  appHolder.innerHTML = ""
  com.raquo.laminar.api.L.render(
    appHolder,
    RoutingStuff.app,
  )

}
