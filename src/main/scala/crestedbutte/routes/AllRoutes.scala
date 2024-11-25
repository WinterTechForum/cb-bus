package crestedbutte.routes

import crestedbutte.{ComponentData, PlanViewer}
import crestedbutte.laminar.AppMode

object AllRoutes {

  def components(
    appMode: AppMode,
  ): Seq[ComponentData] = {
    val basicComponents =
//      mtnExpressRoutes.routesWithTimes ++:
      Seq(
        RtaNorthbound.fullSchedule,
        RtaSouthbound.fullSchedule,
        PlanViewer,
      )
    if (appMode == AppMode.dev)
      basicComponents // + other under-developed features
    else
      basicComponents
  }

}
