package crestedbutte.dom

import crestedbutte.Browser
import org.scalajs.dom.Element
import org.scalajs.dom.raw.MouseEvent
import zio.Runtime.default
import zio.{Task, Unsafe, ZIO}

object BulmaBehaviorLocal {

  def addMenuBehavior(
    input: ZIO[Any, Throwable, Unit],
  ): ZIO[Browser.Service, Nothing, Option[Element]] =
    ZIO
      .service[Browser.Service]
      .map { browser =>
        browser
          .querySelector(
            "#main-menu",
          )
          .map { element =>
            println("selected main menu")
//            new DefaultRuntime {}
//              .unsafeRun(hideOnClickOutside(element, browser))

            browser
              .convertNodesToList(
                element.querySelectorAll(".navbar-item .route"),
              )
              .foreach { node =>
                node.addEventListener(
                  "click",
                  (_: MouseEvent) => {
                    val targetRoute =
                      node.attributes
                        .getNamedItem("data-route")
                        .value
                    browser.rewriteCurrentUrl("route", targetRoute)
                    browser
                      .querySelector("#navbarBasicExample")
                      .foreach(_.classList.remove("is-active"))
                    Unsafe.unsafe(implicit unsafe =>
                      default.unsafe.run(input),
                    )
                  },
                )
              }

            element
          }
      }

  // This isn't really Bulma specific, rather than the .is-active class
  def hideOnClickOutside(
    element: Element,
    browser: Browser.Service,
  ): Task[Unit] =
    ZIO.attempt {
      println("setting up click-outside-menu behavior")
      def outsideClickListener(): MouseEvent => Unit =
        (_: MouseEvent) => {
          println(
            "you might have clicked outside of the main-menu!",
          )
          // TODO Get rid of terrible cast! It probably doesn't even work anyways!
//            if (!element.contains(*clickedElement*) && isVisible(
          if (
            isVisible(
              element,
            )
          ) {
            println(
              "Just going to close the menu because you clicked on *anything*",
            )
            element.classList.remove("is-active")
            removeClickListener() // TODO Make unsafe behavior more explicit
          }
        }

      def removeClickListener() =
        () =>
          browser
            .window()
            .document
            .removeEventListener("click", outsideClickListener())

      browser
        .window()
        .document
        .addEventListener("click", outsideClickListener())
    }

  val isVisible = (element: Element) =>
    element.classList.contains("is-active")

}
