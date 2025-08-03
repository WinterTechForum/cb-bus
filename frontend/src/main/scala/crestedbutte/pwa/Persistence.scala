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

  def getCurrentPlan = {
    val previouslyStoredPlan =
      localStorage
        .getItem("today")

    if (previouslyStoredPlan == null)
      val blankState = Option(Plan(Seq.empty)) // Ugh, wart
      localStorage.setItem("today", blankState.toJson)
      blankState
    else
      previouslyStoredPlan
        .fromJson[Option[Plan]]
        .getOrElse:
          println(
            "Bad plan in localStorage. This can happen after serialization changes. \n" + previouslyStoredPlan,
          )
          val blankState = Option(Plan(Seq.empty)) // Ugh, wart
          localStorage.setItem("today", blankState.toJson)
          blankState
  }

  def updateDailyPlan(
    routeLeg: RouteSegment,
  ) =

    val retrieved =
      getCurrentPlan
        .getOrElse(Plan(Seq.empty))
    val updated =
      retrieved.copy(retrieved.l :+ routeLeg)
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
