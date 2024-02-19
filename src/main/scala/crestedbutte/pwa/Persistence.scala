package crestedbutte.pwa

import com.raquo.laminar.api.L.*
import crestedbutte.*
import org.scalajs.dom
import org.scalajs.dom.window
import zio.json.*

class Persistence():

  val localStorage = window.localStorage

  def createDb() =
    try {
      val retrieved = 
        localStorage.getItem("today")
          .fromJson[Option[Plan]]
          .getOrElse(throw new Exception("Bad plan in localStorage"))
    } catch {
      case e: Exception =>
        println("Error retrieving existing plan: " + e)
        val opt: Option[Plan] = None
        localStorage.setItem("today", opt.toJson)
    }
//    localStorage.setItem("today", null)
//    if (localStorage.getItem("today") == null)
//      localStorage.setItem("today", crestedbutte.Plan(Seq.empty).toJson)
    ()


  def retrieveDailyPlan(
    $plan: Var[Option[Plan]],
  ) =
    val result = localStorage.getItem("today").fromJson[Option[Plan]]
      .getOrElse(throw new Exception("Bad plan in localStorage"))
    $plan.set(result)

  def updateDailyPlan(
    routeLeg: RouteLeg,
  ) =

    val retrieved = 
      localStorage
        .getItem("today")
        .fromJson[Option[Plan]]
        .getOrElse(throw new Exception("Bad plan in localStorage"))
        .getOrElse(throw new Exception("No plan in localStorage"))
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
