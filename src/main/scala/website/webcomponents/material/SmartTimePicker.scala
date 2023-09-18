package website.webcomponents.material

import com.raquo.laminar.api.L._
import com.raquo.laminar.nodes.ReactiveHtmlElement
import com.raquo.laminar.tags.HtmlTag
import org.scalajs.dom

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

object SmartTimePicker {

  @js.native
  trait RawElement extends js.Object {

    def doThing(
    ): Unit // Note: This is not actually implemented in mwc-button, just an example
  }

  @js.native
//  @JSImport("smart-webcomponents/components/smart-timepicker", JSImport.Default)
//  @JSImport("node_modues/smart-webcomponents/modules/smart-timepicker", JSImport.Default)
//  @JSImport("smart-webcomponents/modules/smart-timepicker", JSImport.Default)
//  @JSImport("@material/mwc-linear-progress", JSImport.Default)
  @JSImport("smart-webcomponents", "smart-time-picker")
//  @JSImport("smart-webcomponents/smart-timepicker", JSImport.Default)
  object RawImport extends js.Object

  // object-s are lazy so you need to actually use them in your code to prevent dead code elimination
  RawImport

  type Ref = dom.html.Element with RawElement

  type ModFunction =
    SmartTimePicker.type => Mod[ReactiveHtmlElement[Ref]]

  private val junk = "blah"

  private val tag =
    new HtmlTag[Ref]("smart-time-picker", void = false)

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
//    import com.raquo.domtypes.generic.keys.Style // Laminar aliases ReactiveStyle as Style, but we want the original underlying type here

    val mdcThemePrimary =
      new Style("--mdc-theme-primary", Seq("--mdc-theme-primary"))
  }

  def apply(
    mods: ModFunction*,
  ): HtmlElement =
    tag(mods.map(_(SmartTimePicker)): _*)

}
