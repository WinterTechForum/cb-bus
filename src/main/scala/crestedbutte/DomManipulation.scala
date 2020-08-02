package crestedbutte

import org.scalajs.dom.html.Div
import org.scalajs.dom.raw.Node
import scalatags.JsDom
import zio.{Has, ZIO}
import crestedbutte.Browser.Browser

object DomManipulation {
  import scalatags.JsDom.all._

  def createAndApplyPageStructure(
    pageMode: AppMode.Value,
    componentData: Seq[ComponentData],
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
              TagsOnlyLocal
                .overallPageLayout(pageMode, componentData)
                .render,
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

  def updateUpcomingBusSectionInsideElement(
    elementName: String,
    newContent: JsDom.TypedTag[Div],
    contentContainerId: String,
  ): ZIO[Browser, Nothing, Unit] =
    ZIO
      .access[Browser](_.get)
      .map {
        browser =>
          browser
            .querySelector(s"#$elementName") // TODO Handle case where this is missing
            .foreach {
              routeElementResult =>
                routeElementResult
                  .querySelector("#" + contentContainerId)
                  .innerHTML = ""

                routeElementResult.setAttribute(
                  "style",
                  "display:box",
                ) // TODO or grid?

                routeElementResult
                  .querySelector("#" + contentContainerId)
                  .appendChild(newContent.render)
            }
      }

  def hideUpcomingBusSectionInsideElement(
    elementName: String,
  ): ZIO[Browser, Nothing, Unit] =
    ZIO
      .access[Browser](_.get)
      .map {
        browser =>
          browser
            .querySelector(s"#$elementName") // TODO Handle case where this is missing
            .foreach {
              routeElementResult =>
                routeElementResult.setAttribute(
                  "style",
                  "display:none",
                ) // Come up with way of hiding and collapsing
//            routeElementResult.innerHTML = ""
            }
      }

}
