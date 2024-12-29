package crestedbutte

import java.net.URI

import org.scalajs.dom.{Element, Node, NodeList, Window}
import org.scalajs.dom.raw.HTMLElement
// import zio.Has

object Browser {
  type Browser = Browser.Service
  trait Service {
    def body(): HTMLElement
    def window(): Window
    def querySelector(selectors: String): Option[Element]
    def querySelectorAll(selectors: String): Seq[Element]
    def workOnFullHtmlElement(function: (Element) => Unit): Unit
    def rewriteCurrentUrl(paramName: String, paramValue: String): Unit

    def alterUrlWithNewValue(url: String,
                             paramName: String,
                             paramValue: String): Unit
    def url(): URI
    def convertNodesToList(nodes: NodeList[Element]): Seq[Element] // TODO THIS DOES NOT BELONG HERE
  }
}

trait Browser {
  def browser: Browser.Service
}
