package crestedbutte

import org.scalajs.dom

object MyApp extends App {

  ServiceWorkerClient.registerServiceWorker()
  val appHolder = dom.document.getElementById("landing-message")
  appHolder.innerHTML = ""
  com.raquo.laminar.api.L.render(
    appHolder,
    RoutingStuff.app,
  )

}
