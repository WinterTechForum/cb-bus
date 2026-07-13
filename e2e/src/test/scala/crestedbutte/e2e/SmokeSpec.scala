package crestedbutte.e2e

import com.microsoft.playwright.{Browser, Page, Playwright}
import com.microsoft.playwright.options.WaitForSelectorState
import zio.test.*

import java.io.File
import java.nio.file.{Files, Paths}

/** End-to-end smoke test: boots the real compiled Scala.js app in a headless
  * browser and asserts it renders. Produces a screenshot for visual inspection.
  *
  * Run with: `./sbt e2e/test`
  */
object SmokeSpec extends ZIOSpecDefault:

  private val resourceRoot: File =
    new File(
      sys.props.getOrElse(
        "app.resources",
        "frontend/src/main/resources",
      ),
    )

  // Forked tests run with the e2e module base as their working directory, so
  // this resolves to `e2e/screenshots`.
  private val screenshotDir = Paths.get("screenshots")

  def spec = suite("app smoke")(
    test("app boots and renders the main #container") {
      val server = StaticServer.start(resourceRoot)
      val pw     = Playwright.create()
      try
        val browser: Browser = pw.chromium().launch()
        val page: Page       = browser.newPage()
        page.navigate(s"${server.baseUrl}/index.html")

        // #container is rendered only after the Scala.js app boots, so waiting
        // for it proves the app actually came alive (not just the static shell).
        page.waitForSelector(
          "#container",
          new Page.WaitForSelectorOptions()
            .setState(WaitForSelectorState.ATTACHED)
            .setTimeout(15000),
        )

        val containerCount = page.querySelectorAll("#container").size

        Files.createDirectories(screenshotDir)
        page.screenshot(
          new Page.ScreenshotOptions()
            .setPath(screenshotDir.resolve("smoke.png"))
            .setFullPage(true),
        )

        browser.close()
        assertTrue(containerCount == 1)
      finally
        pw.close()
        server.stop()
    },
  )
