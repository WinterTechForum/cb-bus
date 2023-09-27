package crestedbutte.routes

import crestedbutte.{ComponentData, PlanViewer, TripPlannerComponent}
import crestedbutte.laminar.AppMode

object AllRoutes {

  val mtnExpressRoutes =
    /*
    new CompanyRoutes("Mtn Express", // Peak-season
                      Seq(
                        TownShuttleTimes,
                        CrystalCastleShuttle,
                        ColumbineLoop,
                        SnodgrassShuttle,
                        ThreeSeasonsTimes,
                      )
    )

     */

    new CompanyRoutes("Mtn Express", // Off-season
                      Seq(
                        SpringFallLoop,
                      ),
    )

  def components(
    appMode: AppMode,
  ): Seq[ComponentData] = {
    val basicComponents =
//      mtnExpressRoutes.routesWithTimes ++:
      Seq(
        RtaNorthbound.fullSchedule,
        RtaSouthbound.fullSchedule,
        TripPlannerComponent,
        PlanViewer
      )
    if (appMode == AppMode.dev)
      basicComponents // + other under-developed features
    else
      basicComponents
  }

}
