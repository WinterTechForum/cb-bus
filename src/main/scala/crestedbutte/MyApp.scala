package crestedbutte

import org.scalajs.dom

object MyApp extends App {

  println("Doing this without zio!")
  ServiceWorkerClient.registerServiceWorker()
  val appHolder = dom.document.getElementById("landing-message")
  appHolder.innerHTML = ""
  com.raquo.laminar.api.L.render(
    appHolder,
    RoutingStuff.app,
  )

}
