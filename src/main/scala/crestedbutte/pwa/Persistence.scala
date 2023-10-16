package crestedbutte.pwa

import com.billding.time.{MinuteDuration, WallTime}
import com.raquo.laminar.nodes.ReactiveHtmlElement
import crestedbutte.NotificationStuff.desiredAlarms
import crestedbutte.*
import crestedbutte.dom.BulmaLocal
import crestedbutte.routes.{
  AllRoutes,
  RtaSouthbound,
  SpringFallLoop,
  TownShuttleTimes,
}
import org.scalajs.dom

import java.time.format.{DateTimeFormatter, FormatStyle}
import java.time.{Clock, OffsetDateTime}
import scala.concurrent.duration.FiniteDuration
import crestedbutte.dom.BulmaLocal.ModalMode
import org.scalajs.dom.{
  window,
  IDBDatabase,
  IDBEvent,
  IDBTransactionMode,
  IDBValue,
}
import com.raquo.laminar.api.L._

import zio.json._

class Persistence(
  tripDb: Var[Option[IDBDatabase]]):

  def createDb() =
    if (tripDb.now().isEmpty)
      val dbRequest =
        window.indexedDB.get.open("CbBus", 6L)

      dbRequest.onsuccess = (db: IDBEvent[IDBDatabase]) =>
        tripDb.set(Some(db.target.result))

      dbRequest.onupgradeneeded = (db: IDBEvent[IDBDatabase]) =>
        println("Existing object stores: ")
        db.target.result.objectStoreNames.foreach(println)

        if (db.target.result.objectStoreNames.contains("dailyPlans"))
          println("Deleting object store")
          db.target.result.deleteObjectStore("dailyPlans")

        println("Creating object store")
        db.target.result.createObjectStore("dailyPlans")
        tripDb.set(Some(db.target.result))
        println("created object store")

  def retrieveDailyPlan(
    $plan: Var[Plan],
  ) =
    println("Retrieving daily plan")
    println("tripDb.now(): " + tripDb.now())

    tripDb
      .now()
      .foreach: tripDbLocal =>
        println("non-null DB. Let's try and save")
        val transaction =
          tripDbLocal.transaction("dailyPlans",
                                  IDBTransactionMode.readwrite,
          )
        val objectStore = transaction.objectStore("dailyPlans")
        val request = objectStore.get("today")
        request.onsuccess = (db: IDBEvent[IDBValue]) => {
          println("Retrieved item: " + db.target.result)
          val retrieved = db.target.result.toString.fromJson[Plan]
          $plan.set(
            retrieved.getOrElse(crestedbutte.Plan(Seq.empty)),
          )
          println("Retrieved item: " + retrieved)
        }

  def updateDailyPlan(
    routeLeg: RouteLeg,
  ) =
    tripDb
      .now()
      .foreach: tripDbLocal =>
        val transaction =
          tripDbLocal.transaction("dailyPlans",
                                  IDBTransactionMode.readwrite,
          )
        val objectStore = transaction.objectStore("dailyPlans")
        val request = objectStore.get("today")
        request.onsuccess = (dbEvent: IDBEvent[IDBValue]) =>

          val retrieved =
            dbEvent.target.result.toString.fromJson[Plan]
          val plan = retrieved.getOrElse(Plan(Seq.empty))
          val updatedPlan = plan.copy(plan.legs :+ routeLeg)
          println("Successfully retrieved saved plan: " + plan)
          saveDailyPlanOnly(updatedPlan)
          println("Saved new plan: " + updatedPlan)

  def saveDailyPlan(
    plan: Plan,
  ) =
    Observer { _ =>
      println("Saving daily plan")

      tripDb
        .now()
        .foreach: tripDbLocal =>
          println("non-null DB. Let's try and save!")
          val transaction =
            tripDbLocal.transaction("dailyPlans",
                                    IDBTransactionMode.readwrite,
            )
          println("Persistence A")
          val objectStore = transaction.objectStore("dailyPlans")
          println("Persistence B")
          val request = objectStore.put(plan.toJson, "today")
          println("Persistence C")
          request.onsuccess = (event: dom.Event) =>
            println("Successfully added plan to dailyPlans!")

    }

  def saveDailyPlanOnly(
    plan: Plan,
  ) =
    println("Saving daily plan")

    tripDb
      .now()
      .foreach: tripDbLocal =>
        println("non-null DB. Let's try and save!")
        val transaction =
          tripDbLocal.transaction("dailyPlans",
                                  IDBTransactionMode.readwrite,
          )
        println("Persistence A")
        val objectStore = transaction.objectStore("dailyPlans")
        println("Persistence B")
        val request = objectStore.put(plan.toJson, "today")
        println("Persistence C")
        request.onsuccess = (event: dom.Event) =>
          println("Successfully added plan to dailyPlans!")
