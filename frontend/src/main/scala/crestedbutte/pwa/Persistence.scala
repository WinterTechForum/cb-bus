package crestedbutte.pwa

import com.raquo.laminar.api.L.*
import crestedbutte.*
import org.scalajs.dom
import org.scalajs.dom.window
import zio.json.*

case class Draft(plan: Plan, savedPlanId: Option[String], date: String) derives JsonCodec

class Persistence():


  private val PlansIndexKey = "plans:index"
  private val PlanKeyPrefix = "plan:"
  private val SavedPlansIndexKey = "savedplans:index"
  private val SavedPlanKeyPrefix = "savedplan:"
  private val ScheduleLockedKey = "schedule:locked"
  private val CurrentSavedPlanIdKey = "current:savedplanid"

  val storageProblem: Var[Option[String]] = Var(None)

  private def safelyWrite(key: String, value: String): Unit =
    try
      window.localStorage.setItem(key, value)
    catch
      case _: Exception => storageProblem.set(Some("Couldn't save on this device. Keep this page open and share your trip as text to keep a copy."))

  private def readItem(key: String): String =
    try window.localStorage.getItem(key)
    catch
      case _: Exception =>
        storageProblem.set(Some("Device storage is unavailable. This trip cannot be restored after closing."))
        null

  private def removeItem(key: String): Unit =
    try window.localStorage.removeItem(key)
    catch
      case _: Exception => storageProblem.set(Some("Couldn't update saved data on this device."))

  def getDraft: Option[Draft] =
    Option(readItem("draft:v1")).flatMap { raw =>
      raw.fromJson[Draft].toOption match
        case Some(draft) => Some(draft)
        case None =>
          safelyWrite("draft:recovery", raw)
          None
    }

  def saveDraft(plan: Plan, savedPlanId: Option[String], date: String): Unit =
    safelyWrite("draft:v1", Draft(plan, savedPlanId, date).toJson)

  /** A shared URL is an import, not an instruction to reset edits on each reload. */
  def importSharedPlan(plan: Plan, date: String): Unit =
    clearCurrentSavedPlanId()
    saveDraft(plan, None, date)

  def stopPreferences(key: String): List[Location] =
    Option(readItem(key)).flatMap(_.fromJson[List[Location]].toOption).getOrElse(Nil)

  def saveStopPreferences(key: String, stops: List[Location]): Unit =
    safelyWrite(key, stops.distinct.toJson)

  def rememberStop(stop: Location): Unit =
    saveStopPreferences("stops:recent", (stop :: stopPreferences("stops:recent")).take(5))

  // ===== Schedule locked state =====
  def getScheduleLocked: Boolean =
    val raw = readItem(ScheduleLockedKey)
    if raw == null then false
    else raw == "true"

  def setScheduleLocked(
    locked: Boolean,
  ): Unit = safelyWrite(ScheduleLockedKey, locked.toString)

  // ===== Current SavedPlan ID state =====
  /** Get the ID of the currently loaded SavedPlan, if any.
    */
  def getCurrentSavedPlanId: Option[String] =
    val raw = readItem(CurrentSavedPlanIdKey)
    if raw == null || raw.isEmpty then None
    else Some(raw)

  /** Set the ID of the currently loaded SavedPlan.
    */
  def setCurrentSavedPlanId(
    id: String,
  ): Unit = safelyWrite(CurrentSavedPlanIdKey, id)

  /** Clear the current SavedPlan ID (when creating a new plan or
    * clearing).
    */
  def clearCurrentSavedPlanId(): Unit =
    removeItem(CurrentSavedPlanIdKey)

  /** Get the currently loaded SavedPlan, if any.
    */
  def getCurrentSavedPlan: Option[SavedPlan] =
    getCurrentSavedPlanId.flatMap(getSavedPlan)

  private def planStorageKey(
    name: String,
  ): String = s"${PlanKeyPrefix}${name}"

  private def readPlanNamesIndex(): List[String] =
    val raw = readItem(PlansIndexKey)
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
  ): Unit = safelyWrite(PlansIndexKey, names.toJson)

  def initializeOrResetStorage() =
    getCurrentPlan
    ()

  // ===== Existing single-plan ("today") APIs =====
  def getCurrentPlan = {
    val previouslyStoredPlan =
      readItem("today")

    if (previouslyStoredPlan == null)
      val blankState = Option(Plan(Seq.empty)) // Ugh, wart
      safelyWrite("today", blankState.toJson)
      blankState
    else
      previouslyStoredPlan
        .fromJson[Option[Plan]]
        .getOrElse:
          // Retain the damaged record for recovery; never clear unrelated trips.
          safelyWrite("today:recovery", previouslyStoredPlan)
          val blankState = Option(Plan(Seq.empty)) // Ugh, wart
          safelyWrite("today", blankState.toJson)
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
      safelyWrite("today", plan.toJson)
      $plan.set(plan)
    }

  def saveDailyPlanOnly(
    plan: Plan,
  ) = safelyWrite("today", plan.toJson)

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
    val raw = readItem(key)
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
    safelyWrite(key, Option(plan).toJson)
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
    removeItem(key)
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

    if readItem(newKey) != null then
      throw new IllegalArgumentException(
        s"A plan named '${newName}' already exists",
      )

    val existing = readItem(oldKey)
    if existing == null then
      // Nothing to rename; ensure index is clean
      writePlanNamesIndex(
        readPlanNamesIndex().filterNot(_ == oldName),
      )
    else
      safelyWrite(newKey, existing)
      removeItem(oldKey)
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

  /** Purge all legacy name-based plans from localStorage. This should
    * be called after migration to UUID-based plans is complete.
    */
  def purgeLegacyNamedPlans(): Unit =
    val legacyNames = readPlanNamesIndex()
    // Delete each plan entry
    legacyNames.foreach { name =>
      val key = planStorageKey(name)
      removeItem(key)
    }
    // Clear the index
    removeItem(PlansIndexKey)
    println(
      s"Purged ${legacyNames.size} legacy name-based plans from localStorage",
    )

  // ===== New UUID-based SavedPlan APIs =====

  private def savedPlanStorageKey(
    id: String,
  ): String = s"${SavedPlanKeyPrefix}${id}"

  private def readSavedPlanIdsIndex(): List[String] =
    val raw = readItem(SavedPlansIndexKey)
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
  ): Unit = safelyWrite(SavedPlansIndexKey, ids.toJson)

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
    val raw = readItem(key)
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
  ): Boolean =
    val key = savedPlanStorageKey(savedPlan.id)
    safelyWrite(key, savedPlan.toJson)
    val current = readSavedPlanIdsIndex()
    if !current.contains(savedPlan.id) then
      writeSavedPlanIdsIndex(current :+ savedPlan.id)
    getSavedPlan(savedPlan.id).contains(savedPlan) && listSavedPlanIds().contains(savedPlan.id)

  /** Delete a saved plan by ID.
    */
  def deleteSavedPlan(
    id: String,
  ): Unit =
    val key = savedPlanStorageKey(id)
    removeItem(key)
    val current = readSavedPlanIdsIndex()
    if current.contains(id) then
      writeSavedPlanIdsIndex(current.filterNot(_ == id))
