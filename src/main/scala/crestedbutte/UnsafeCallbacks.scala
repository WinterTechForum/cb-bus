package crestedbutte

import org.scalajs.dom.Node
import org.scalajs.dom.raw.MouseEvent
import zio.ZIO
import crestedbutte.Browser.Browser
import org.scalajs.dom.Element

object UnsafeCallbacks {

  val attachMenuBehavior: ZIO[Browser, Nothing, Unit] =
    ZIO
      .service[Browser]
      .map(browser =>
        browser
          .window()
          .document
          .addEventListener(
            "DOMContentLoaded",
            (_: Any) => {

              def menuCallbackBehavior(
                node: Element,
                browser: Browser.Service,
              ) =
                (_: MouseEvent) =>
                  // Get the target from the "data-target" attribute
                  // POTENTIALLY VERY EXPENSIVE. It's jumping back to the root of the document with this search.
                  browser
                    .querySelector(
                      "#" + node.attributes
                        .getNamedItem("data-target")
                        .value,
                    )
                    .map(_.classList.toggle("is-active"))

              browser
                .querySelectorAll(".navbar-burger")
                .foreach(node =>
                  node
                    .addEventListener(
                      "click",
                      menuCallbackBehavior(node, browser),
                    ),
                )

            },
          ),
      )

}
