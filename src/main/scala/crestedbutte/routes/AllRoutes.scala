package crestedbutte.routes

import crestedbutte.laminar
import crestedbutte.laminar.AppMode
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
  ): Seq[ComponentData] = {
    val basicComponents =
      mtnExpressRoutes.routesWithTimes ++:
      Seq(
        RtaNorthbound.fullSchedule,
        RtaSouthbound.fullSchedule,
        RoundTripCalculatorComponent,
      )
    if (appMode == AppMode.dev)
      basicComponents // + other under-developed features
    else
      basicComponents
  }

}
