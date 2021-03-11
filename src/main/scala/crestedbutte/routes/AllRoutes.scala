package crestedbutte.routes

import crestedbutte.laminar
import crestedbutte.laminar.AppMode.AppMode
import crestedbutte.laminar.{
  AppMode,
  ComponentData,
  LaminarRoundTripCalculator,
  RoundTripCalculatorComponent,
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
      mtnExpressRoutes.routesWithTimes ++:
      Seq(
        RtaNorthbound.fullSchedule,
        RtaSouthbound.fullSchedule,
        RoundTripCalculatorComponent,
      )
    else
      mtnExpressRoutes.routesWithTimes ++:
      Seq(
        RtaNorthbound.fullSchedule,
        RtaSouthbound.fullSchedule,
      )

}
