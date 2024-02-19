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

  def retrieveDailyPlan(
    $plan: Var[Option[Plan]],
  ) =
    val result = localStorage
      .getItem("today")
      .fromJson[Option[Plan]]
      .getOrElse(throw new Exception("Bad plan in localStorage"))
    $plan.set(result)
    result

  def retrieveDailyPlanOnly =
    println("retrieveDailyPlanOnly")
    localStorage
      .getItem("today")
      .fromJson[Option[Plan]]
      .getOrElse(throw new Exception("Bad plan in localStorage"))

  def updateDailyPlan(
    routeLeg: RouteLeg,
  ) =

    val retrieved =
      localStorage
        .getItem("today")
        .fromJson[Option[Plan]]
        .getOrElse(throw new Exception("Bad plan in localStorage"))
    val updated =
      retrieved match
        case Some(value) =>
          value.copy(value.legs :+ routeLeg)
        case None => Plan(Seq(routeLeg))
    saveDailyPlanOnly(updated)
    println("Saved new plan: " + updated)

  def saveDailyPlan(
    plan: Plan,
    $plan: Var[Option[Plan]],
  ) =
    Observer { _ =>
      val newPlan: Option[Plan] = Some(plan)
      localStorage.setItem("today", newPlan.toJson)
      $plan.set(newPlan)
    }

  def saveDailyPlanOnly(
    plan: Plan,
  ) =
    val newPlan: Option[Plan] = Some(plan)
    localStorage.setItem("today", newPlan.toJson)

    val retrieved =
      localStorage
        .getItem("today")
        .fromJson[Option[Plan]]
        .getOrElse(throw new Exception("Bad plan in localStorage"))
    println("Saved new plan: " + retrieved)
    retrieved
