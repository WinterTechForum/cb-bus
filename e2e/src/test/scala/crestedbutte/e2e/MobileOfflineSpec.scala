package crestedbutte.e2e

import com.microsoft.playwright.Page
import zio.test.*
import E2ESupport.*

object MobileOfflineSpec extends ZIOSpecDefault {
  private def draft(page: Page): String =
    page.evaluate("() => localStorage.getItem('draft:v1')").toString

  def spec = suite("mobile trips offline")(
    test("first visit downloads the shell and restores an unnamed draft at a new offline URL") {
      withApp { (page, baseUrl) =>
        page.setViewportSize(390, 844)
        loadSelector(page, baseUrl)
        planSingleSegment(page)
        val before = draft(page)
        page.waitForFunction("() => document.querySelector('.offline-status').textContent.includes('Available offline')")
        page.context().setOffline(true)
        page.navigate(s"$baseUrl/index.html?time=10:00&unvisited=offline")
        page.waitForSelector(PlannedSegment)
        val restored = draft(page)
        val stillEditable = page.getByText("Change time").isVisible
        assertTrue(before == restored, plannedSegmentCount(page) == 1, stillEditable)
      }
    },
    test("saved trip edits recover after reload without overwriting the reusable template") {
      withApp { (page, baseUrl) =>
        loadSelector(page, baseUrl)
        planSingleSegment(page)
        page.locator(".bottom-bar-name--tappable").click()
        page.locator(".bottom-bar-name-input").fill("Commute")
        page.locator(".bottom-bar-save-btn").click()
        val saved = page.evaluate("() => localStorage.getItem('savedplan:' + localStorage.getItem('current:savedplanid'))").toString
        page.getByText("Change time").click()
        page.locator(".departure-select").selectOption("12:20")
        page.getByText("Use this departure").click()
        val edited = draft(page)
        navigate(page, baseUrl)
        page.waitForSelector(PlannedSegment)
        val templateUnchanged = page.evaluate("() => localStorage.getItem('savedplan:' + localStorage.getItem('current:savedplanid'))").toString == saved
        assertTrue(draft(page) == edited, templateUnchanged,
          page.locator(".departure-times").innerText().contains("12:20"))
      }
    },
    test("use now retimes a saved ride and leaves saved data alone until Save") {
      withApp { (page, baseUrl) =>
        loadSelector(page, baseUrl)
        planSingleSegment(page)
        page.getByText("Change time").click()
        page.locator(".departure-select").selectOption("12:20")
        page.getByText("Use this departure").click()
        page.locator(".bottom-bar-name--tappable").click()
        page.locator(".bottom-bar-name-input").fill("Afternoon")
        page.locator(".bottom-bar-save-btn").click()
        val saved = page.evaluate("() => localStorage.getItem('savedplan:' + localStorage.getItem('current:savedplanid'))").toString
        page.getByText("Use this trip now").click()
        val unchanged = page.evaluate("() => localStorage.getItem('savedplan:' + localStorage.getItem('current:savedplanid'))").toString == saved
        assertTrue(unchanged, page.locator(".departure-times").innerText().contains("10:20"))
      }
    },
    test("damaged current-plan data does not erase saved trips") {
      withApp { (page, baseUrl) =>
        loadSelector(page, baseUrl)
        planSingleSegment(page)
        page.locator(".bottom-bar-name--tappable").click()
        page.locator(".bottom-bar-name-input").fill("Keep me")
        page.locator(".bottom-bar-save-btn").click()
        page.evaluate("() => { localStorage.setItem('today', 'broken'); localStorage.setItem('draft:v1', 'broken'); }")
        navigate(page, baseUrl)
        page.waitForSelector(PlannedSegment)
        assertTrue(page.locator(".bottom-bar-name").innerText() == "Keep me",
          page.evaluate("() => localStorage.getItem('draft:recovery')").toString == "broken")
      }
    },
    test("landmark search and favorites survive reopening offline") {
      withApp { (page, baseUrl) =>
        loadSelector(page, baseUrl)
        page.locator(".stop-search").fill("walmart")
        val found = page.locator(".stop-name").innerText()
        page.locator(".favorite-stop").click()
        page.waitForFunction("() => document.querySelector('.offline-status').textContent.includes('Available offline')")
        page.context().setOffline(true)
        navigate(page, baseUrl)
        page.waitForSelector(".stop-group")
        assertTrue(found == "Spencer & Highway 135",
          page.locator(".stop-group").first().locator("h3").innerText() == "Favorites",
          page.locator(".stop-group").first().locator(".stop-name").innerText() == found)
      }
    },
  )
}
