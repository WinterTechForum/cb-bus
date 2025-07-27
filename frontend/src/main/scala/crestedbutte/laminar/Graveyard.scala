package crestedbutte.laminar

import com.raquo.laminar.api.L.*
import crestedbutte.LateNightRecommendation

object Graveyard {

  def SafeRideLink(
    safeRideRecommendation: LateNightRecommendation,
  ) =
    div(
      cls := "late-night-call-button",
      a(
        href := s"tel:${safeRideRecommendation.phoneNumber}",
        cls := "link",
        button(
          cls := "button",
          SvgIcon
            .call()
            .amend(
              alt := "Call Late Night Shuttle!",
            ),
          safeRideRecommendation.message,
        ),
      ),
    )

}
