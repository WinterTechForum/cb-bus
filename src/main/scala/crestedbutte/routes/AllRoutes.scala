package crestedbutte.routes

import crestedbutte.laminar
import crestedbutte.laminar.AppMode.AppMode
import crestedbutte.laminar.{
  AppMode,
  ComponentData,
  ComponentDataRoute,
  ComponentDataTyped,
  LaminarRoundTripCalculator,
}

object AllRoutes {

  val mtnExpressRoutes =
    new CompanyRoutes("Mtn Express",
                      Seq(
                        TownShuttleTimes,
                        CrystalCastleShuttle,
                        ColumbineLoop,
                        SnodgrassShuttle,
                        ThreeSeasonsTimes,
                      ))

  def components(
    appMode: AppMode,
  ): Seq[ComponentData] =
    if (appMode == AppMode.Development)
      mtnExpressRoutes.routesWithTimes
        .map(ComponentDataRoute) ++:
      Seq(
        laminar.ComponentDataRoute(
          RtaNorthbound.fullSchedule,
        ),
        laminar.ComponentDataRoute(
          RtaSouthbound.fullSchedule,
        ),
        ComponentDataTyped(
          "RoundTripCalculator",
          LaminarRoundTripCalculator.calculatorComponentName,
        ),
      )
    else
      mtnExpressRoutes.routesWithTimes
        .map(ComponentDataRoute) ++:
      Seq(
        laminar.ComponentDataRoute(
          RtaNorthbound.fullSchedule,
        ),
        laminar.ComponentDataRoute(
          RtaSouthbound.fullSchedule,
        ),
      )

}
