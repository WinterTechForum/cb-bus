package crestedbutte.e2e

import com.microsoft.playwright.{Browser, Page, Playwright}
import com.microsoft.playwright.options.WaitForSelectorState
import scala.jdk.CollectionConverters.*

import java.io.File

/** Shared plumbing for the browser-driven e2e specs: lifecycle management,
  * deterministic app loading (frozen clock), and reusable DOM interactions like
  * "discover a workable stop pair" and "plan a single segment".
  */
object E2ESupport:

  val resourceRoot: File =
    new File(sys.props.getOrElse("app.resources", "frontend/src/main/resources"))

  /** `?time=HH:mm` freezes the app clock to Fri 2025-02-21 (dense winter
    * service) so the schedule is deterministic regardless of wall-clock. */
  val FrozenTimeUrl = "/?time=10:00"

  val StopButtons    = "button[id^='stop-btn-']"
  val PlannedSegment = ".plan-segments"
  val RouteGap       = ".route-gap"

  def cssForId(id: String): String = s"[id=\"$id\"]"

  /** A planned first leg and the identifiers/names involved. */
  final case class PlannedLeg(
    originId: String,
    destId: String,
    originName: String,
    destName: String,
  )

  /** Full browser lifecycle. `body` receives a ready page and the base URL.
    * The context is pinned to America/Denver so the frozen Mountain-time clock
    * aligns with local-time schedule rendering. */
  def withApp[A](body: (Page, String) => A): A =
    val server = StaticServer.start(resourceRoot)
    val pw     = Playwright.create()
    try
      val browser = pw.chromium().launch()
      val context =
        browser.newContext(
          new Browser.NewContextOptions().setTimezoneId("America/Denver"),
        )
      val page = context.newPage()
      try body(page, server.baseUrl)
      finally
        context.close()
        browser.close()
    finally
      pw.close()
      server.stop()

  /** Navigate to the frozen-clock app and wait for the Scala.js app to boot. */
  def navigate(page: Page, baseUrl: String): Unit =
    page.navigate(s"$baseUrl$FrozenTimeUrl")
    page.waitForSelector(
      "#container",
      new Page.WaitForSelectorOptions().setTimeout(15000),
    )

  /** Wait for the staggered stop-button list to fully render and settle. */
  def awaitStopButtons(page: Page): Unit =
    page.waitForSelector(
      StopButtons,
      new Page.WaitForSelectorOptions().setTimeout(10000),
    )
    var previous = -1
    var current  = page.querySelectorAll(StopButtons).size
    while current != previous do
      previous = current
      page.waitForTimeout(200)
      current = page.querySelectorAll(StopButtons).size

  /** Load app + wait for the stop selector (origin-selection state). */
  def loadSelector(page: Page, baseUrl: String): Unit =
    navigate(page, baseUrl)
    awaitStopButtons(page)

  def headerText(page: Page): String =
    Option(page.querySelector("h2")).map(_.textContent().trim).getOrElse("")

  /** (id, disabled) for every currently rendered stop button. */
  def stopButtonState(page: Page): List[(String, Boolean)] =
    page
      .querySelectorAll(StopButtons)
      .asScala
      .toList
      .map(h => h.getAttribute("id") -> h.isDisabled)

  def plannedSegmentCount(page: Page): Int =
    page.querySelectorAll(PlannedSegment).size

  def routeGapCount(page: Page): Int =
    page.querySelectorAll(RouteGap).size

  /** Poll a condition until true or timeout; returns whether it became true. */
  def pollUntil(page: Page, timeoutMs: Int = 8000)(cond: => Boolean): Boolean =
    val deadline = System.currentTimeMillis() + timeoutMs
    var ok       = cond
    while !ok && System.currentTimeMillis() < deadline do
      page.waitForTimeout(100)
      ok = cond
    ok

  /** From origin-selection state, find an origin that has at least one reachable
    * destination at the frozen time. Leaves the app back on origin selection. */
  def discoverWorkablePair(page: Page): Option[(String, String)] =
    val originIds = stopButtonState(page).map(_._1)
    var result: Option[(String, String)] = None
    val it = originIds.iterator
    while result.isEmpty && it.hasNext do
      val originId = it.next()
      page.locator(cssForId(originId)).click()
      page.waitForTimeout(150) // let disabled states settle
      val destO =
        stopButtonState(page).collectFirst {
          case (id, disabled) if !disabled && id != originId => id
        }
      destO match
        case Some(destId) => result = Some(originId -> destId)
        case None         => ()
      page.locator(cssForId(originId)).click() // deselect, restore clean state
      page.waitForTimeout(100)
    result

  /** Plan one segment with the minimal two touches and wait for it to render. */
  def planSingleSegment(page: Page): PlannedLeg =
    val (originId, destId) =
      discoverWorkablePair(page).getOrElse(
        throw new AssertionError(
          s"No workable origin/destination pair at $FrozenTimeUrl",
        ),
      )
    val originName = page.locator(cssForId(originId)).textContent().trim
    page.locator(cssForId(originId)).click()
    pollUntil(page)(headerText(page) == "Select your destination")
    val destName = page.locator(cssForId(destId)).textContent().trim
    page.locator(cssForId(destId)).click()
    page.waitForSelector(
      PlannedSegment,
      new Page.WaitForSelectorOptions()
        .setState(WaitForSelectorState.ATTACHED)
        .setTimeout(10000),
    )
    PlannedLeg(originId, destId, originName, destName)

  /** Open the trip menu ("+") and tap "Continue from last stop". */
  def continueFromLastStop(page: Page): Unit =
    page.waitForSelector(
      ".floating-center-button",
      new Page.WaitForSelectorOptions().setTimeout(8000),
    )
    page.locator(".floating-center-button").click()
    page.getByText("Continue from last stop").click()
