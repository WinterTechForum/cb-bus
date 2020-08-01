package crestedbutte.dom

import crestedbutte.Browser
import org.scalajs.dom.{Element, Event}
import org.scalajs.dom.raw.MouseEvent
import zio.{Has, IO, Task, ZIO}
import zio.Runtime.default

object BulmaBehaviorLocal {

  def addMenuBehavior(
    input: ZIO[Any, Throwable, Unit],
  ): ZIO[Has[Browser.Service], Nothing, Option[Element]] =
    ZIO
      .access[Has[Browser.Service]](_.get)
      .map {
        browser =>
          browser
            .querySelector(
              "#main-menu",
            )
            .map {
              element =>
                println("selected main menu")
//            new DefaultRuntime {}
//              .unsafeRun(hideOnClickOutside(element, browser))

                browser
                  .convertNodesToList(
                    element.querySelectorAll(".navbar-item .route"),
                  )
                  .foreach {
                    node =>
                      node.addEventListener(
                        "click",
                        (_: MouseEvent) => {
                          val targetRoute =
                            node.attributes
                              .getNamedItem("data-route")
                              .value
                          if (browser
                                .url()
                                .getPath
                                .contains("index_dev"))
                            browser.rewriteCurrentUrl("route",
                                                      targetRoute)
                          else
                            browser
                              .alterUrlWithNewValue("/index.html",
                                                    "route",
                                                    targetRoute)
                          browser
                            .querySelector("#navbarBasicExample")
                            .foreach(_.classList.remove("is-active"))
                          println("should do menu stuff now...")
                          default.unsafeRunAsync(input)(_ => ())
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
    ZIO {
      println("setting up click-outside-menu behavior")
      def outsideClickListener(): MouseEvent => Unit =
        (event: MouseEvent) => {
          println(
            "you might have clicked outside of the main-menu!",
          )
          // TODO Get rid of terrible cast! It probably doesn't even work anyways!
//            if (!element.contains(*clickedElement*) && isVisible(
          if (isVisible(
                element,
              )) {
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
