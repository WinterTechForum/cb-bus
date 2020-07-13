package crestedbutte

import org.scalajs.dom.html.Div
import org.scalajs.dom.raw.Node
import scalatags.JsDom
import zio.{Has, ZIO}

object DomManipulation {
  import scalatags.JsDom.all._

  def createAndApplyPageStructure(
    pageMode: AppMode.Value,
    componentData: Seq[ComponentData],
  ): ZIO[Has[Browser.Service], Nothing, Node] =
    ZIO
      .access[Has[Browser.Service]](_.get)
      .map { browser =>
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
  ): ZIO[Has[Browser.Service], Throwable, Unit] =
    ZIO
      .access[Has[Browser.Service]](_.get)
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
  ): ZIO[Has[Browser.Service], Nothing, Unit] =
    ZIO
      .access[Has[Browser.Service]](_.get)
      .map { browser =>
        browser
          .querySelector(s"#$elementName") // TODO Handle case where this is missing
          .foreach { routeElementResult =>
            routeElementResult
              .querySelector("#upcoming-buses")
              .innerHTML = ""

            routeElementResult.setAttribute("style", "display:box") // TODO or grid?

            routeElementResult
              .querySelector("#upcoming-buses")
              .appendChild(newContent.render)
          }
      }

  def hideUpcomingBusSectionInsideElement(
    elementName: String,
  ): ZIO[Has[Browser.Service], Nothing, Unit] =
    ZIO
      .access[Has[Browser.Service]](_.get)
      .map { browser =>
        browser
          .querySelector(s"#$elementName") // TODO Handle case where this is missing
          .foreach { routeElementResult =>
            routeElementResult.setAttribute(
              "style",
              "display:none",
            ) // Come up with way of hiding and collapsing
//            routeElementResult.innerHTML = ""
          }
      }

}
