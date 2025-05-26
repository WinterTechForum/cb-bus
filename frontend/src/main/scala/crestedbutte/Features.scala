package crestedbutte

enum Feature:
  case MapLinks, BusAlarms

case class FeatureStatus(
  feature: Feature,
  enabled: Boolean)

case class FeatureSets(
  values: Map[Feature, Boolean]) {

  def isEnabled(
    feature: Feature,
  ): Boolean = values(feature) // Unsafe
  def update(
    featureStatus: FeatureStatus,
  ): FeatureSets =
    copy(
      values = values + (kv =
        (featureStatus.feature, featureStatus.enabled),
      ),
    )
}
