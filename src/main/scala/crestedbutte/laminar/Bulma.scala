package crestedbutte.laminar

import com.raquo.laminar.api.L._
import com.raquo.laminar.nodes.ReactiveHtmlElement
import org.scalajs.dom
import org.scalajs.dom.html
import com.raquo.laminar.nodes.ReactiveHtmlElement
import crestedbutte.ComponentData
import org.scalajs.dom
import org.scalajs.dom.html

class Bulma {}

object Bulma {

  def menu(
    selectedRoute: Var[ComponentData], // TODO Use this.
    components: Seq[ComponentData],
  ): ReactiveHtmlElement[html.Div] = {

    val menuClicks = new EventBus[dom.Event]

    val activeStyling =
      menuClicks.events.foldLeft("")(
        (acc, next) =>
          if (!acc.contains("is-active")) "is-active" else "",
      )

    def menuButton(
      componentData: ComponentData,
    ) =
      a(
        cls := "navbar-item route  using-library",
        onClick.map(event => componentData) --> selectedRoute.writer,
        dataAttr("route") := s"${componentData.componentName.name}",
        componentData.componentName.userFriendlyName,
      )

    val routeButtons = components.map(menuButton)

    div(
      idAttr := "main-menu",
      cls := "navbar",
      role := "navigation",
      aria.label := "main navigation",
      div(
        cls := "navbar-brand",
        a(
          role := "button",
          cls := "navbar-burger burger",
          onClick --> menuClicks,
          aria.label := "menu",
          aria.expanded(false),
          dataAttr("target") := "navbarBasicExample",
          span(aria.hidden(true)),
          span(aria.hidden(true)),
          span(aria.hidden(true)),
        ),
      ),
      div(
        idAttr := "navbarBasicExample",
        cls := "navbar-menu",
        cls <-- activeStyling,
        div(
          cls := "navbar-start",
          div(
            hr(cls := "navbar-divider"),
            div(
              cls := "navbar-item has-dropdown is-hoverable",
              a(onClick --> menuClicks,
                cls("navbar-link centered is-size-4"),
                "Routes"),
              div(cls("navbar-dropdown"), routeButtons),
            ),
          ),
        ),
        div(cls("navbar-end")),
      ),
    )
  }
}
