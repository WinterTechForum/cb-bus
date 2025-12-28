package crestedbutte.laminar

import com.raquo.laminar.api.L.*

object SvgIcon {

  def apply(
    name: String,
    clsName: String = "",
  ) =
    img(
      cls := s"glyphicon $clsName",
      src := s"/glyphicons/svg/individual-svg/$name",
    )

  // Predefined icon methods
  def arrowDown(
    clsName: String = "",
  ) = apply("glyphicons-basic-211-arrow-down.svg", clsName)

  def squareMinus(
    clsName: String = "",
  ) = apply("glyphicons-basic-842-square-minus.svg", clsName)

  def call(
    clsName: String = "",
  ) = apply("glyphicons-basic-465-call.svg", clsName)

  def map(
    clsName: String = "",
  ) = apply("glyphicons-basic-592-map.svg", clsName)

  def share(
    clsName: String = "",
  ) = apply("glyphicons-basic-324-share.svg", clsName)

  def times(
    clsName: String = "",
  ) = apply("glyphicons-basic-373-times.svg", clsName)

  /** Heart icon using a span with heart character for "interested" voting */
  def heart(
    clsName: String = "",
  ) =
    span(
      cls := s"icon-heart $clsName",
      "♥",
    )

}
