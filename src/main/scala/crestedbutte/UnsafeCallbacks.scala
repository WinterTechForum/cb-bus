package crestedbutte

import org.scalajs.dom.Node
import org.scalajs.dom.raw.MouseEvent
import zio.{Has, ZIO}

object UnsafeCallbacks {

  val attachMenuBehavior: ZIO[Has[Browser.Service], Nothing, Unit] =
    ZIO
      .access[Has[Browser.Service]](_.get)
      .map(
        browser =>
          browser
            .window()
            .document
            .addEventListener(
              "DOMContentLoaded",
              (_: Any) => {

                def menuCallbackBehavior(
                  node: Node,
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
                  .foreach(
                    node =>
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
