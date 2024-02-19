package crestedbutte.pwa

import com.raquo.laminar.api.L.*
import crestedbutte.*
import org.scalajs.dom
import org.scalajs.dom.window
import zio.json.*

class Persistence():

  val localStorage = window.localStorage

  def createDb() =
    if (localStorage.getItem("today") == null)
      localStorage.setItem("today", crestedbutte.Plan(Seq.empty).toJson)

  def retrieveDailyPlan(
    $plan: Var[Plan],
  ) =
    val result = localStorage.getItem("today").fromJson[Plan]
      .getOrElse(throw new Exception("Bad plan in localStorage"))
    $plan.set(result)

  def updateDailyPlan(
    routeLeg: RouteLeg,
  ) =

    val retrieved = localStorage.getItem("today").fromJson[Plan].getOrElse(throw new Exception("Bad plan in localStorage"))
    val updatedPlan = retrieved.copy(retrieved.legs :+ routeLeg)
    saveDailyPlanOnly(updatedPlan)
    println("Saved new plan: " + updatedPlan)

  def saveDailyPlan(
    plan: Plan,
  ) =
    Observer { _ =>
      localStorage.setItem("today", plan.toJson)
    }

  def saveDailyPlanOnly(
    plan: Plan,
  ) =
      localStorage.setItem("today", plan.toJson)
