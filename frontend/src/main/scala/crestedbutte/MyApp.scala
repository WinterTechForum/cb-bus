package crestedbutte

import crestedbutte.laminar.AppMode

import org.scalajs.dom

object MyApp extends App {
  // ServiceWorkerClient.registerServiceWorker() // TODO Restore before pushing

  val appHolder = dom.document.getElementById("landing-message")
  appHolder.innerHTML = ""
  com.raquo.laminar.api.L.render(
    appHolder,
    RoutingStuff.app,
  )

}
