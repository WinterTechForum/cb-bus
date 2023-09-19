package crestedbutte

// This class should be usable *without* needing access to the Clock.
case class UpcomingArrivalInfo(
  location: Location,
  content: Either[StopTimeInfo, LateNightRecommendation],
  /* TODO: waitDuration: Duration*/)

object UpcomingArrivalInfo {

  def apply(
    location: Location,
    content: StopTimeInfo,
  ): UpcomingArrivalInfo =
    UpcomingArrivalInfo(
      location,
      Left(
        content,
      ),
    )

  def apply(
    location: Location,
    content: LateNightRecommendation,
  ): UpcomingArrivalInfo =
    UpcomingArrivalInfo(
      location,
      Right(content),
    )

}
