package crestedbutte.e2e

import com.microsoft.playwright.{Browser, ElementHandle, Page, Playwright}
import com.microsoft.playwright.options.WaitForSelectorState
import zio.test.*

import java.io.File
import scala.jdk.CollectionConverters.*

/** Locks in the *minimum number of touches* to plan a single-segment route.
  *
  * Two phases:
  *   1. EXPLORE — with the clock frozen to a dense-service time, discover a
  *      working origin→destination pair at runtime (so the test survives
  *      schedule edits instead of hard-coding stop names).
  *   2. MEASURE — on a clean page, drive that pair one touch at a time and
  *      assert the boundary: 1 touch is NOT enough, 2 touches IS. That pins the
  *      minimum at exactly 2 for regression purposes — a new confirmation step
  *      (→ 3) or an accidental one-tap plan (→ 1) both break this test.
  *
  * Run with: `./sbt e2e/test`
  */
object SingleSegmentPlanningSpec extends ZIOSpecDefault:

  private val resourceRoot: File =
    new File(sys.props.getOrElse("app.resources", "frontend/src/main/resources"))

  // Built-in test hook: `?time=HH:mm` freezes the app clock to this time on
  // Fri 2025-02-21 (ski season → frequent service), making the schedule
  // deterministic regardless of when the suite actually runs.
  private val FrozenTimeUrl = "/?time=10:00"

  private val StopButtons     = "button[id^='stop-btn-']"
  private val PlannedSegment  = ".plan-segments"

  private def cssForId(id: String): String = s"[id=\"$id\"]"

  private def headerText(page: Page): String =
    Option(page.querySelector("h2")).map(_.textContent().trim).getOrElse("")

  /** ids of the currently rendered stop buttons, with their disabled state. */
  private def stopButtonState(page: Page): List[(String, Boolean)] =
    page
      .querySelectorAll(StopButtons)
      .asScala
      .toList
      .map(h => h.getAttribute("id") -> h.isDisabled)

  private def plannedSegmentCount(page: Page): Int =
    page.querySelectorAll(PlannedSegment).size

  private def loadFreshApp(page: Page, baseUrl: String): Unit =
    page.navigate(s"$baseUrl$FrozenTimeUrl")
    page.waitForSelector(
      "#container",
      new Page.WaitForSelectorOptions().setTimeout(15000),
    )
    // Stop buttons stream in on a ~30ms-per-item stagger; wait for the list to
    // settle so enumeration sees every option.
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

  def spec = suite("single-segment planning")(
    test("planning a single-segment route takes exactly two touches") {
      val server = StaticServer.start(resourceRoot)
      val pw     = Playwright.create()
      try
        val browser: Browser = pw.chromium().launch()
        // America/Denver so the frozen Mountain-time clock lines up with the
        // browser's local-time schedule rendering.
        val context =
          browser.newContext(
            new Browser.NewContextOptions().setTimezoneId("America/Denver"),
          )
        val page = context.newPage()

        // ---- Phase 1: EXPLORE — find a workable origin→destination pair ----
        loadFreshApp(page, server.baseUrl)

        val originIds = stopButtonState(page).map(_._1)
        var workablePair: Option[(String, String)] = None
        val originIter = originIds.iterator
        while workablePair.isEmpty && originIter.hasNext do
          val originId = originIter.next()
          page.locator(cssForId(originId)).click() // tentative origin

          val destO =
            stopButtonState(page)
              .collectFirst {
                case (id, disabled) if !disabled && id != originId => id
              }
          destO match
            case Some(destId) => workablePair = Some(originId -> destId)
            case None         =>
              // Dead end: deselect this origin and try the next one.
              page.locator(cssForId(originId)).click()

        // ---- Phase 2: MEASURE — drive the pair one touch at a time ----
        val (originId, destId) =
          workablePair.getOrElse(
            throw new AssertionError(
              "Exploration found no origin with a reachable destination at " +
                s"$FrozenTimeUrl — pick a frozen time with active service.",
            ),
          )

        // Clean slate: clear any persisted state and reload.
        page.evaluate("() => { localStorage.clear(); }")
        loadFreshApp(page, server.baseUrl)

        // 0 touches
        val headerAtStart      = headerText(page)
        val plannedAtZeroTouch = plannedSegmentCount(page)

        // 1 touch — origin only
        page.locator(cssForId(originId)).click()
        val headerAfterOne    = headerText(page)
        page.waitForTimeout(300) // give any (unexpected) segment time to render
        val plannedAtOneTouch = plannedSegmentCount(page)

        // 2 touches — destination
        page.locator(cssForId(destId)).click()
        page.waitForSelector(
          PlannedSegment,
          new Page.WaitForSelectorOptions()
            .setState(WaitForSelectorState.ATTACHED)
            .setTimeout(10000),
        )
        val plannedAtTwoTouch = plannedSegmentCount(page)

        val minimumTouches = 2

        context.close()
        browser.close()

        assertTrue(
          // Exploration confirmed the flow before we measured it.
          workablePair.isDefined,
          // Baseline: nothing planned, sitting on origin selection.
          headerAtStart == "Select your origin",
          plannedAtZeroTouch == 0,
          // One touch advances to destination selection but plans nothing.
          headerAfterOne == "Select your destination",
          plannedAtOneTouch == 0,
          // Two touches plans exactly one segment — the locked-in minimum.
          plannedAtTwoTouch == 1,
          minimumTouches == 2,
        )
      finally
        pw.close()
        server.stop()
    },
  )
