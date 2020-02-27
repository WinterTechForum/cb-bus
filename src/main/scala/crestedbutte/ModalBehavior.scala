package crestedbutte

import org.scalajs.dom.raw.{
  Element,
  MouseEvent,
  NamedNodeMap,
  NodeList
}
import zio.{DefaultRuntime, ZIO}

object ModalBehavior {

  def clipRootHtml(element: Element) =
    element.classList.add("is-clipped")

  def id(value: String) =
    "#" + value

  val addModalOpenBehavior =
    ZIO
      .environment[Browser]
      .map { browser =>
        def activateModal(targetName: String): Unit =
          org.scalajs.dom.document.body // TODO from environment.
            .querySelector(targetName)
            .classList
            .add("is-active")

        // TODO function that iterates over elements when passed a selector
        val modalOpenButtons: NodeList = browser.browser
          .window()
          .document
          .querySelectorAll(".open-arrival-time-modal")

        for { i <- Range(0, modalOpenButtons.length) } {
          println("about to add modal open Button behavior")
          modalOpenButtons
            .item(i)
            .addEventListener(
              "click",
              (clickEvent: MouseEvent) => {
                val modalContentId =
                  modalOpenButtons
                    .item(i)
                    .attributes
                    .getNamedItem("data-schedule-modal")
                    .value

                clickEvent.preventDefault();

                val modal: Element = org.scalajs.dom.document.body
                  .querySelector(
                    id(modalContentId)
                  )

                clipRootHtml(
                  org.scalajs.dom.document
                    .querySelector("html")
                )

                println(
                  "about to add modal background click behavior"
                )
                modal
                  .querySelector(".modal-background")
                  .addEventListener(
                    "click",
                    (e: MouseEvent) => {
                      e.preventDefault();

                      removeClippedHtml(browser)

                      modal.classList.remove("is-active");
                    }
                  );

                activateModal(
                  id(modalContentId)
                )
              }
            )
        }
      }

  def removeClippedHtml(browser: Browser) =
    browser.browser
      .body()
      .querySelector("html")
      .classList
      .remove("is-clipped")

  val addModalCloseBehavior =
    ZIO
      .environment[Browser]
      .map { browser =>
        val modalCloseButtons = browser.browser
          .window()
          .document
          .querySelectorAll(".modal-close")
        for { i <- Range(0, modalCloseButtons.length) } {
          println("about to add modalCloseButton")
          modalCloseButtons
            .item(i)
            .addEventListener(
              "click",
              (mouseEvent: MouseEvent) => {

                org.scalajs.dom.document
                  .querySelector("html")
                  .classList
                  .remove("is-clipped");
                if (browser.browser
                      .window()
                      .document
                      .querySelector(".is-active") != null) {
                  browser.browser
                    .window()
                    .document
                    .querySelector(".is-active")
                    .classList
                    .remove("is-active")
                }
              }
            )

        }
      }

}