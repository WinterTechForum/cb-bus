package crestedbutte

object GpsCalculations {

  def degreesToRadians(
    degrees: Double,
  ) =
    degrees * Math.PI / 180

  def distanceInKmBetweenEarthCoordinatesT(
    position1: GpsCoordinates,
    position2: GpsCoordinates,
  ): Double =
    distanceInKmBetweenEarthCoordinates(position1.latitude,
                                        position1.longitude,
                                        position2.latitude,
                                        position2.longitude)

  def distanceInKmBetweenEarthCoordinates(
    lat1: Double,
    lon1: Double,
    lat2: Double,
    lon2: Double,
  ): Double = {
    val earthRadiusKm = 6371

    val dLat = degreesToRadians(lat2 - lat1)
    val dLon = degreesToRadians(lon2 - lon1)

    val lat1rads = degreesToRadians(lat1)
    val lat2rads = degreesToRadians(lat2)

    val a = Math.sin(dLat / 2) * Math.sin(dLat / 2) +
      Math.sin(dLon / 2) * Math.sin(dLon / 2) * Math.cos(lat1rads) * Math
        .cos(lat2rads);
    val c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a))
    earthRadiusKm * c
  }

}
