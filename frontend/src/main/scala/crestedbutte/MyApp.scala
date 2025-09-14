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
  val plan =
    Plan(
      Seq(
        RouteSegment
          .attempt(
            RTA.Southbound.componentName,
            LocationWithTime(Location.BrushCreek, WallTime("11:01")),
            LocationWithTime(Location.Almont, WallTime("11:24")),
          )
          .getOrElse(???),
      ),
    )
  val plan2 =
    Plan(
      Seq(
        RouteSegment
          .attempt(
            RTA.Southbound.componentName,
            LocationWithTime(Location.BrushCreek, WallTime("11:31")),
            LocationWithTime(Location.Almont, WallTime("11:54")),
          )
          .getOrElse(???),
      ),
    )
  val persistence = Persistence()
  val saveSymbols = Map(
    "floppy"   -> "💾",
    "download" -> "⬇️",
    "bookmark" -> "🔖",
    "check"    -> "✅",
  )
  persistence.savePlanByName("Test", plan)
  persistence.savePlanByName("Test2", plan2)
  com.raquo.laminar.api.L.render(
    appHolder,
    RoutingStuff.app,
  )

}
