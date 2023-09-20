package crestedbutte

import enumeratum._

sealed trait Feature extends EnumEntry

// TODO Turn into Scala 3 enum
object Feature extends Enum[Feature] {
  val values = findValues // macro

  case object MapLinks extends Feature
  case object BusAlarms extends Feature
}

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
      values =
        values + (kv = (featureStatus.feature, featureStatus.enabled)),
    )
}
