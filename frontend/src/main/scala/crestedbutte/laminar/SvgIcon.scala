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

}
