package crestedbutte

import crestedbutte.laminar.OfflineStatus
import org.scalajs.dom
import scala.scalajs.js
import scala.scalajs.js.timers.*
import scala.concurrent.ExecutionContext.Implicits.global

object ServiceWorkerClient {
  def registerServiceWorker(): Unit = {
    val navigator = dom.window.navigator.asInstanceOf[js.Dynamic]
    if (js.isUndefined(navigator.serviceWorker)) {
      OfflineStatus.problem.set(Some("Offline installation is unavailable in this browser."))
      return
    }
    val workers = navigator.serviceWorker
    var applyUpdate = false

    def checkReady(): Unit = {
      val controller = workers.controller
      if (controller != null && !js.isUndefined(controller)) {
        val channel = new dom.MessageChannel()
        val timeout = setTimeout(10000) {
          channel.port1.close()
          if (!OfflineStatus.ready.now())
            OfflineStatus.problem.set(Some("Offline download not confirmed. Reopen while connected to finish setup."))
        }
        channel.port1.onmessage = (event: dom.MessageEvent) => {
          clearTimeout(timeout)
          val ready = event.data.asInstanceOf[js.Dynamic].ready.asInstanceOf[Boolean]
          OfflineStatus.ready.set(ready)
          OfflineStatus.problem.set(if (ready) None else Some("Offline download incomplete. Reconnect and reopen the app."))
          channel.port1.close()
        }
        controller.postMessage("OFFLINE_STATUS", js.Array(channel.port2))
      }
    }

    workers.oncontrollerchange = { (_: dom.Event) =>
      OfflineStatus.update.set(None)
      if (applyUpdate) dom.window.location.reload()
      else checkReady()
    }: js.Function1[dom.Event, Unit]

    val registrationFuture = workers.register("/sw.js", js.Dynamic.literal(scope = "/", updateViaCache = "none"))
      .asInstanceOf[js.Promise[js.Dynamic]].toFuture
    registrationFuture.foreach { registration =>
        def offerUpdate(): Unit = {
          val waiting = registration.waiting
          if (waiting != null && !js.isUndefined(waiting) && workers.controller != null) {
            OfflineStatus.update.set(Some(() => {
              applyUpdate = true
              waiting.postMessage("ACTIVATE_UPDATE")
            }))
          }
        }
        offerUpdate()
        def observeInstallation(): Unit = {
          val installing = registration.installing
          if (installing != null) {
            installing.onstatechange = { (_: dom.Event) =>
              val state = installing.state.asInstanceOf[String]
              if (state == "installed") { offerUpdate(); checkReady() }
              if (state == "redundant" && !OfflineStatus.ready.now())
                OfflineStatus.problem.set(Some("Offline download failed. Reconnect and reopen the app to retry."))
            }: js.Function1[dom.Event, Unit]
          }
        }
        registration.onupdatefound = { (_: dom.Event) => observeInstallation() }: js.Function1[dom.Event, Unit]
        observeInstallation()
        checkReady()
        workers.ready.asInstanceOf[js.Promise[js.Dynamic]].toFuture.foreach(_ => checkReady())
      }
    // Registration failures must be visible, including a blocked/private context.
    registrationFuture.failed.foreach { _ =>
      OfflineStatus.problem.set(Some("Offline setup unavailable. Reopen while connected to try again."))
    }
  }
}
