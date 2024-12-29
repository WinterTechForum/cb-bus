package crestedbutte

import crestedbutte.Browser.Browser
import org.scalajs.dom.raw.Node
import zio.ZIO

object DomManipulation {

  def createAndApplyPageStructure(
    pageContent: Node,
  ): ZIO[Browser, Nothing, Node] =
    ZIO
      .service[Browser]
      .map {
        browser =>
          browser
            .querySelector("#landing-message")
            .map(browser.body().removeChild(_))
          browser
            .body()
            .appendChild(
              pageContent,
            )
      }

  def appendMessageToPage(
    message: Node,
  ): ZIO[Browser, Throwable, Unit] =
    ZIO
      .service[Browser]
      .map[Unit](
        browser => {
          println("Should show timezones: " + message)
          browser
            .querySelector(".timezone")
            .foreach(_.appendChild(message))
        },
      )

  def updateContentInsideElementAndReveal(
    containerId: String,
    newContent: Node,
    innerContentId: String,
  ): ZIO[Browser, Nothing, Unit] =
    ZIO
      .service[Browser]
      .map {
        browser =>
          browser
            .querySelector(s"#$containerId") // TODO Handle case where this is missing
            .foreach {
              routeElementResult =>
                routeElementResult
                  .querySelector("#" + innerContentId)
                  .innerHTML = ""

                routeElementResult.setAttribute(
                  "style",
                  "display:box",
                ) // TODO or grid?

                routeElementResult
                  .querySelector("#" + innerContentId)
                  .appendChild(newContent)
            }
      }

  def hideElement(
    elementId: String,
  ): ZIO[Browser, Nothing, Unit] =
    ZIO
      .service[Browser]
      .map {
        browser =>
          browser
            .querySelector(s"#$elementId")
            .foreach {
              routeElementResult =>
                routeElementResult.setAttribute(
                  "style",
                  "display:none",
                ) // Come up with way of hiding and collapsing
            }
      }

}
