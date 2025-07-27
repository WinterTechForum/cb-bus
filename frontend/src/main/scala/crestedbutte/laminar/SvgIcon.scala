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
      alt := "Thanks for riding the bus!",
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

}
