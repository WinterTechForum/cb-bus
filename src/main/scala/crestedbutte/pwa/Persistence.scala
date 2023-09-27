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

object Persistence:
  import com.raquo.laminar.api.L._

  import zio.json._

  def createDb(tripDb: Var[Option[IDBDatabase]]) =
    Observer { _ =>
      if (tripDb.now().isEmpty)
        val dbRequest =
          window.indexedDB.get.open("CbBus", 3L)

        dbRequest.onsuccess = (db: IDBEvent[IDBDatabase]) =>
          println("Assigning DB")
          tripDb.set(Some(db.target.result))
          println("Assigned DB: " + tripDb.now())

        dbRequest.onupgradeneeded = (db: IDBEvent[IDBDatabase]) =>
          println("creating object store")
//          db.target.result.createObjectStore("dailyPlans")
          println("creating object store")
    }

  def saveDailyPlan(
                     plan: Plan,
                     tripDb: Var[Option[IDBDatabase]]
                   ) =
    Observer { _ =>
      println("Saving daily plan")

      tripDb.now().foreach: tripDbLocal =>
        println("non-null DB. Let's try and save")
        val transaction =
          tripDbLocal.transaction("dailyPlans",
            IDBTransactionMode.readwrite,
          )
        val objectStore = transaction.objectStore("dailyPlans")
        val request = objectStore.put(plan.toJson, "today")
        request.onsuccess = (event: dom.Event) =>
          println("Successfully added plan to dailyPlans")

    }

  def retrieveDailyPlan(
                         $plan: Var[Plan],
                         tripDb: Var[Option[IDBDatabase]]
                       ) =
    Observer { _ =>
      println("Retrieving daily plan")
      println("tripDb.now(): " + tripDb.now())

      tripDb.now().foreach: tripDbLocal =>
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
          $plan.set(retrieved.getOrElse(crestedbutte.Plan(Seq.empty)))
          println("Retrieved item: " + retrieved)
        }

    }
