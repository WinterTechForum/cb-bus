package crestedbutte.pwa

import com.raquo.laminar.api.L.*
import crestedbutte.*
import org.scalajs.dom
import org.scalajs.dom.window
import zio.json.*

class Persistence():

  val localStorage = window.localStorage

  private val PlansIndexKey = "plans:index"
  private val PlanKeyPrefix = "plan:"
  private val SavedPlansIndexKey = "savedplans:index"
  private val SavedPlanKeyPrefix = "savedplan:"
  private val ScheduleLockedKey = "schedule:locked"

  // ===== Schedule locked state =====
  def getScheduleLocked: Boolean =
    val raw = localStorage.getItem(ScheduleLockedKey)
    if raw == null then false
    else raw == "true"

  def setScheduleLocked(
    locked: Boolean,
  ): Unit = localStorage.setItem(ScheduleLockedKey, locked.toString)

  private def planStorageKey(
    name: String,
  ): String = s"${PlanKeyPrefix}${name}"

  private def readPlanNamesIndex(): List[String] =
    val raw = localStorage.getItem(PlansIndexKey)
    if raw == null then List.empty
    else
      raw
        .fromJson[List[String]]
        .getOrElse:
          println(
            "Bad plans index in localStorage; defaulting to empty index",
          )
          List.empty

  private def writePlanNamesIndex(
    names: List[String],
  ): Unit = localStorage.setItem(PlansIndexKey, names.toJson)

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

  // ===== Existing single-plan ("today") APIs =====
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

  // ===== New multi-plan (named) APIs =====

  /** List all saved plan names.
    */
  def listPlanNames(): List[String] = readPlanNamesIndex()

  /** Retrieve a named plan. Returns None if not found or if the
    * stored value is invalid.
    */
  def getPlanByName(
    name: String,
  ): Option[Plan] =
    val key = planStorageKey(name)
    val raw = localStorage.getItem(key)
    if raw == null then None
    else
      raw
        .fromJson[Option[Plan]]
        .fold(
          err =>
            println(s"Bad named plan '$name' in localStorage: ${err}")
            None
          ,
          opt => opt,
        )

  /** Create or overwrite a named plan and ensure its name is tracked
    * in the index.
    */
  def savePlanByName(
    name: String,
    plan: Plan,
  ): Unit =
    val key = planStorageKey(name)
    localStorage.setItem(key, Option(plan).toJson)
    val current = readPlanNamesIndex()
    if !current.contains(name) then
      writePlanNamesIndex(current :+ name)

  /** Remove a named plan and update the index accordingly. No-op if
    * it does not exist.
    */
  def deletePlanByName(
    name: String,
  ): Unit =
    val key = planStorageKey(name)
    localStorage.removeItem(key)
    val current = readPlanNamesIndex()
    if current.contains(name) then
      writePlanNamesIndex(current.filterNot(_ == name))

  /** Rename a plan, preserving its contents. Throws if target name
    * already exists.
    */
  def renamePlan(
    oldName: String,
    newName: String,
  ): Unit =
    if oldName == newName then return
    val newKey = planStorageKey(newName)
    val oldKey = planStorageKey(oldName)

    if localStorage.getItem(newKey) != null then
      throw new IllegalArgumentException(
        s"A plan named '${newName}' already exists",
      )

    val existing = localStorage.getItem(oldKey)
    if existing == null then
      // Nothing to rename; ensure index is clean
      writePlanNamesIndex(
        readPlanNamesIndex().filterNot(_ == oldName),
      )
    else
      localStorage.setItem(newKey, existing)
      localStorage.removeItem(oldKey)
      val names = readPlanNamesIndex()
      val updated =
        names.map(n => if n == oldName then newName else n).distinct
      writePlanNamesIndex(updated)

  /** Append a route segment to a named plan (creating a new plan if
    * it does not exist).
    */
  def updatePlanByName(
    name: String,
    routeLeg: RouteSegment,
  ): Unit =
    val existing = getPlanByName(name).getOrElse(Plan(Seq.empty))
    val updated = existing.copy(existing.l :+ routeLeg)
    savePlanByName(name, updated)

  // ===== New UUID-based SavedPlan APIs =====

  private def savedPlanStorageKey(
    id: String,
  ): String = s"${SavedPlanKeyPrefix}${id}"

  private def readSavedPlanIdsIndex(): List[String] =
    val raw = localStorage.getItem(SavedPlansIndexKey)
    if raw == null then List.empty
    else
      raw
        .fromJson[List[String]]
        .getOrElse:
          println(
            "Bad saved plans index in localStorage; defaulting to empty index",
          )
          List.empty

  private def writeSavedPlanIdsIndex(
    ids: List[String],
  ): Unit = localStorage.setItem(SavedPlansIndexKey, ids.toJson)

  /** List all saved plan IDs.
    */
  def listSavedPlanIds(): List[String] = readSavedPlanIdsIndex()

  /** Retrieve a saved plan by ID. Returns None if not found or if the
    * stored value is invalid.
    */
  def getSavedPlan(
    id: String,
  ): Option[SavedPlan] =
    val key = savedPlanStorageKey(id)
    val raw = localStorage.getItem(key)
    if raw == null then None
    else
      raw
        .fromJson[SavedPlan]
        .fold(
          err =>
            println(s"Bad saved plan '$id' in localStorage: ${err}")
            None
          ,
          sp => Some(sp),
        )

  /** List all saved plans (loads each one).
    */
  def listSavedPlans(): List[SavedPlan] =
    readSavedPlanIdsIndex().flatMap(getSavedPlan)

  /** Save a SavedPlan (create or update). The ID is used as the
    * storage key.
    */
  def saveSavedPlan(
    savedPlan: SavedPlan,
  ): Unit =
    val key = savedPlanStorageKey(savedPlan.id)
    localStorage.setItem(key, savedPlan.toJson)
    val current = readSavedPlanIdsIndex()
    if !current.contains(savedPlan.id) then
      writeSavedPlanIdsIndex(current :+ savedPlan.id)

  /** Delete a saved plan by ID.
    */
  def deleteSavedPlan(
    id: String,
  ): Unit =
    val key = savedPlanStorageKey(id)
    localStorage.removeItem(key)
    val current = readSavedPlanIdsIndex()
    if current.contains(id) then
      writeSavedPlanIdsIndex(current.filterNot(_ == id))
