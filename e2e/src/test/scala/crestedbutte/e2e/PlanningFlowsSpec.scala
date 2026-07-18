package crestedbutte.e2e

import com.microsoft.playwright.Page
import com.microsoft.playwright.options.WaitForSelectorState
import zio.test.*
import scala.jdk.CollectionConverters.*

import E2ESupport.*

/** User-level planning flows beyond the single-segment minimum:
  *   - chaining a second leg (transfer) into a multi-segment plan
  *   - the "Continue from last stop" shortcut pre-selecting the next origin
  *   - clearing the plan returning to origin selection
  *   - the reachability guard disabling unreachable destinations
  *
  * Run with: `./sbt e2e/test`
  */
object PlanningFlowsSpec extends ZIOSpecDefault:

  def spec = suite("planning flows")(
    test("chaining a second leg produces a two-segment plan with a gap") {
      withApp { (page, baseUrl) =>
        loadSelector(page, baseUrl)
        val leg1 = planSingleSegment(page)

        // Add a second leg starting where the first ended.
        continueFromLastStop(page)
        pollUntil(page)(headerText(page) == "Select your destination")
        awaitStopButtons(page)

        // Pick any reachable onward stop (not the pre-selected origin).
        val onwardO =
          stopButtonState(page).collectFirst {
            case (id, disabled) if !disabled && id != leg1.destId => id
          }
        val onwardId = onwardO.getOrElse(
          throw new AssertionError(
            s"No onward stop reachable from ${leg1.destName} at $FrozenTimeUrl",
          ),
        )
        page.locator(cssForId(onwardId)).click()

        // Two segments now render, joined by exactly one gap.
        pollUntil(page)(plannedSegmentCount(page) == 2)
        val segments = plannedSegmentCount(page)
        val gaps     = routeGapCount(page)

        assertTrue(
          onwardO.isDefined,
          segments == 2,
          gaps == 1,
        )
      }
    },
    test("moving a segment down reorders the trip") {
      withApp { (page, baseUrl) =>
        loadSelector(page, baseUrl)
        val leg1 = planSingleSegment(page)

        // Build a second leg so there's something to reorder.
        continueFromLastStop(page)
        pollUntil(page)(headerText(page) == "Select your destination")
        awaitStopButtons(page)
        val onwardId =
          stopButtonState(page)
            .collectFirst {
              case (id, disabled) if !disabled && id != leg1.destId => id
            }
            .getOrElse(throw new AssertionError("no onward stop"))
        page.locator(cssForId(onwardId)).click()
        pollUntil(page)(plannedSegmentCount(page) == 2)

        def legLabels: List[String] =
          page
            .querySelectorAll(".plan-segments_left")
            .asScala
            .toList
            .map(el =>
              Option(el.querySelector("div"))
                .map(_.textContent().trim)
                .getOrElse(""),
            )

        val before = legLabels

        // Tap the down arrow (2nd reorder-btn) on the first segment.
        page
          .locator(".plan-segments")
          .first()
          .locator("button.reorder-btn")
          .nth(1)
          .click()

        // For two legs, moving the first down swaps their order.
        val reordered = pollUntil(page)(legLabels == before.reverse)

        assertTrue(before.size == 2, reordered)
      }
    },
    test("'Continue from last stop' pre-selects the previous destination as origin") {
      withApp { (page, baseUrl) =>
        loadSelector(page, baseUrl)
        val leg1 = planSingleSegment(page)

        continueFromLastStop(page)

        // The next selector should skip origin selection entirely: it opens on
        // "Select your destination" with the last stop already chosen — so the
        // second leg costs one fewer touch than the first.
        val advancedToDestination =
          pollUntil(page)(headerText(page) == "Select your destination")
        val originIndicator =
          pollUntil(page)(
            Option(page.querySelector(".origin-indicator-name")).isDefined,
          )
        val indicatorName =
          Option(page.querySelector(".origin-indicator-name"))
            .map(_.textContent().trim)
            .getOrElse("")

        assertTrue(
          advancedToDestination,
          originIndicator,
          indicatorName == leg1.destName,
        )
      }
    },
    test("clearing the plan via 'New trip' returns to origin selection") {
      withApp { (page, baseUrl) =>
        loadSelector(page, baseUrl)
        planSingleSegment(page)
        val plannedBefore = plannedSegmentCount(page)

        // Clear the plan through the bottom-bar menu ("New trip" empties it).
        page.waitForSelector(
          ".bottom-bar-menu-btn",
          new Page.WaitForSelectorOptions().setTimeout(8000),
        )
        page.locator(".bottom-bar-menu-btn").click()
        page.getByText("New trip").click()

        // With the plan cleared, the app drops back to adding a new route.
        // The segment element exits via an animation, so poll until it's gone.
        val returnedToSelector =
          pollUntil(page)(headerText(page) == "Select your origin")
        val segmentsCleared =
          pollUntil(page)(plannedSegmentCount(page) == 0)

        assertTrue(
          plannedBefore == 1,
          returnedToSelector,
          segmentsCleared,
        )
      }
    },
    test("selecting an origin disables destinations that are unreachable at that time") {
      withApp { (page, baseUrl) =>
        loadSelector(page, baseUrl)

        // Before choosing an origin, every stop is selectable as a starting point.
        val headerBefore  = headerText(page)
        val disabledBefore = stopButtonState(page).count(_._2)

        // Find an origin that partitions stops into reachable + unreachable,
        // proving the guard actually constrains choices (not all-or-nothing).
        val originIds = stopButtonState(page).map(_._1)
        var guarding: Option[(String, Int, Int)] = None
        val it = originIds.iterator
        while guarding.isEmpty && it.hasNext do
          val originId = it.next()
          page.locator(cssForId(originId)).click()
          page.waitForTimeout(150)
          val others   = stopButtonState(page).filter(_._1 != originId)
          val disabled = others.count(_._2)
          val enabled  = others.count(!_._2)
          if disabled >= 1 && enabled >= 1 then
            guarding = Some((originId, disabled, enabled))
          else
            page.locator(cssForId(originId)).click() // deselect, try next
            page.waitForTimeout(100)

        val (_, disabledAfter, enabledAfter) = guarding.getOrElse(
          throw new AssertionError(
            s"No origin produced a reachable/unreachable split at $FrozenTimeUrl",
          ),
        )
        val headerAfter = headerText(page)

        assertTrue(
          headerBefore == "Select your origin",
          disabledBefore == 0,           // all stops valid as an origin
          headerAfter == "Select your destination",
          enabledAfter >= 1,             // some destinations reachable
          disabledAfter >= 1,            // others correctly blocked
        )
      }
    },
  )
