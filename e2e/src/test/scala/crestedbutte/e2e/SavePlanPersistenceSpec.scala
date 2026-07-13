package crestedbutte.e2e

import com.microsoft.playwright.Page
import com.microsoft.playwright.options.WaitForSelectorState
import zio.test.*

import E2ESupport.*

/** Locks in the core PWA persistence behaviour: a saved trip survives a full
  * page reload (localStorage-backed), restoring both its segment and its name.
  *
  * Run with: `./sbt e2e/test`
  */
object SavePlanPersistenceSpec extends ZIOSpecDefault:

  private val TripName = "E2E Regression Trip"

  def spec = suite("save + persistence")(
    test("a saved trip is restored after a full page reload") {
      withApp { (page, baseUrl) =>
        loadSelector(page, baseUrl)
        planSingleSegment(page)

        // Save via the bottom bar: open the menu (▼), choose "Save trip",
        // enter a name, and confirm.
        page.waitForSelector(
          ".bottom-bar-menu-btn",
          new Page.WaitForSelectorOptions().setTimeout(8000),
        )
        page.locator(".bottom-bar-menu-btn").click()
        page.getByText("Save trip").click()
        page.waitForSelector(
          ".bottom-bar-name-input",
          new Page.WaitForSelectorOptions().setTimeout(8000),
        )
        page.locator(".bottom-bar-name-input").fill(TripName)
        page.locator(".bottom-bar-save-btn").click()

        // Saved state shows the name (no longer "Unsaved Trip").
        val savedShowsName =
          pollUntil(page)(
            Option(page.querySelector(".bottom-bar-name"))
              .map(_.textContent().trim)
              .contains(TripName),
          )

        // Reload WITHOUT clearing storage — the saved plan should come back.
        navigate(page, baseUrl)
        page.waitForSelector(
          PlannedSegment,
          new Page.WaitForSelectorOptions()
            .setState(WaitForSelectorState.ATTACHED)
            .setTimeout(10000),
        )

        val segmentsAfterReload = plannedSegmentCount(page)
        val nameAfterReload =
          pollUntil(page)(
            Option(page.querySelector(".bottom-bar-name"))
              .map(_.textContent().trim)
              .contains(TripName),
          )

        assertTrue(
          savedShowsName,
          segmentsAfterReload == 1,
          nameAfterReload,
        )
      }
    },
  )
