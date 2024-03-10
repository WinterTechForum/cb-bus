package crestedbutte.pwa

import com.raquo.laminar.api.L.*
import crestedbutte.*
import org.scalajs.dom
import org.scalajs.dom.window
import zio.json.*

class Persistence():

  val localStorage = window.localStorage

  def initializeOrResetStorage() =
    try {
      val retrieved =
        localStorage
          .getItem("today")
          .fromJson[Option[Plan]]
          .getOrElse(throw new Exception("Bad plan in localStorage"))
    }
    catch {
      case e: Exception =>
        println("Error retrieving existing plan: " + e)
        println("Going to nuke all saved data")
        localStorage.clear()
        val opt: Option[Plan] = None
        localStorage.setItem("today", opt.toJson)
    }
    ()

  // TODO Delete after de-option'ing Plan everywhere
//  def retrieveDailyPlan(
//    $plan: Var[Plan],
//  ) =
//    val result = localStorage
//      .getItem("today")
//      .fromJson[Option[Plan]]
//      .getOrElse(throw new Exception("Bad plan in localStorage"))
//      .getOrElse(Plan(Seq.empty))
//    $plan.set(result)
//    result

  def retrieveDailyPlanOnly =
    localStorage
      .getItem("today")
      .fromJson[Option[Plan]]
      .getOrElse(throw new Exception("Bad plan in localStorage"))

  def updateDailyPlan(
    routeLeg: RouteLeg,
  ) =

    val retrieved =
      retrieveDailyPlanOnly
        .getOrElse(Plan(Seq.empty))
    val updated =
      retrieved.copy(retrieved.legs :+ routeLeg)
    saveDailyPlanOnly(updated)

  def saveDailyPlan(
    plan: Plan,
    $plan: Var[Plan],
  ) =
    Observer { _ =>
      localStorage.setItem("today", plan.toJson)
      $plan.set(plan)
    }

  def saveDailyPlanOnly(
    plan: Plan,
  ) = localStorage.setItem("today", plan.toJson)
