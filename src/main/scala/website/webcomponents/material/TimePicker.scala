package website.webcomponents.material

import com.raquo.laminar.api.L._
import com.raquo.laminar.tags._
//import com.raquo.laminar.api.Ht.builders.HtmlTag
import com.raquo.laminar.codecs._
//import com.raquo.laminar.keys._
import com.raquo.laminar.nodes.ReactiveHtmlElement
import org.scalajs.dom

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

object TimePicker {

  @js.native
  trait RawElement extends js.Object {

    def doThing(
    ): Unit // Note: This is not actually implemented in mwc-button, just an example
  }

  @js.native
  @JSImport("@material-ui/pickers", JSImport.Default)
  object RawImport extends js.Object

  // object-s are lazy so you need to actually use them in your code to prevent dead code elimination
  RawImport

  type Ref = dom.html.Element with RawElement
  type ModFunction = TimePicker.type => Mod[ReactiveHtmlElement[Ref]]

  private val tag = new HtmlTag[Ref]("TimePicker", void = false)

  ReactiveHtmlElement
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
//    import com.raquo.domtypes.defs.styles.StyleDefs.generic.keys.Style // Laminar aliases ReactiveStyle as Style, but we want the original underlying type here

    val mdcThemePrimary =
      new Style("--mdc-theme-primary")
  }

  def apply(
    mods: ModFunction*,
  ): HtmlElement = tag(mods.map(_(TimePicker)): _*)

}
