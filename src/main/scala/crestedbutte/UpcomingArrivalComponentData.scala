package crestedbutte

case class UpcomingArrivalComponentData(
  upcomingArrivalInfoForAllRoutes: Seq[
    UpcomingArrivalInfoWithFullSchedule,
  ],
  routeName: RouteName,
)
