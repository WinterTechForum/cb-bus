package website.webcomponents.material

import com.raquo.laminar.api.L._
import com.raquo.laminar.nodes.ReactiveHtmlElement
import com.raquo.laminar.tags.HtmlTag
import org.scalajs.dom

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

object Button {

  @js.native
  trait RawElement extends js.Object {

    def doThing(
    ): Unit // Note: This is not actually implemented in mwc-button, just an example
  }

  @js.native
  @JSImport("@material/mwc-button", JSImport.Default)
  object RawImport extends js.Object

  // object-s are lazy so you need to actually use them in your code to prevent dead code elimination
  RawImport

  type Ref = dom.html.Element with RawElement
  type ModFunction = Button.type => Mod[ReactiveHtmlElement[Ref]]

  private val tag = new HtmlTag[Ref]("mwc-button", void = false)

//  val id: ReactiveProp[String, String] = idAttr

//  val label = new ReactiveHtmlAttr[String]("label", StringAsIsCodec)

//  val raised = new ReactiveHtmlAttr[Boolean](
//    "raised",
//    BooleanAsAttrPresenceCodec,
//  )

//  val icon = new ReactiveHtmlAttr[String]("icon", StringAsIsCodec)

  val onMouseOver = new EventProp[dom.MouseEvent]("mouseover")

  object slots {

    def icon(
      el: HtmlElement,
    ): HtmlElement = el.amend(slot := "icon")
  }

  object styles {

    val mdcThemePrimary =
      new Style("--mdc-theme-primary")
  }

  def apply(
    mods: ModFunction*,
  ): HtmlElement = tag(mods.map(_(Button)): _*)

}
