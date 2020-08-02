package crestedbutte

import org.scalajs.dom.html.Div
import org.scalajs.dom.raw.Node
import scalatags.JsDom
import zio.{Has, ZIO}
import crestedbutte.Browser.Browser

object DomManipulation {
  import scalatags.JsDom.all._

  def createAndApplyPageStructure(
    pageContent: Node,
  ): ZIO[Browser, Nothing, Node] =
    ZIO
      .access[Browser](_.get)
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
    message: String,
  ): ZIO[Browser, Throwable, Unit] =
    ZIO
      .access[Browser](_.get)
      .map[Unit](
        browser => {
          println("Should show timezones: " + message)
          browser
            .querySelector(".timezone")
            .foreach(_.appendChild(div(message).render))
        },
      )

//  def updateIdContentInsideElementAndReveal(
//                                             containerId: String,
//                                             newContent: Node,
//                                             innerContentId: String,
//                                           ): ZIO[Browser, Nothing, Unit] =
  def updateIdContentInsideElementAndReveal(
    containerId: String,
    newContent: Node,
    innerContentId: String,
  ): ZIO[Browser, Nothing, Unit] =
    ZIO
      .access[Browser](_.get)
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
      .access[Browser](_.get)
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
