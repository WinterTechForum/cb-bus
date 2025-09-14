package com.billding.time

case class MinuteDuration(
  minutes: Minutes) {
  val toMinutes: Long = minutes.value

  def humanFriendly = {
    val hoursData =
      if (toMinutes / 60 == 1)
        Some((1, "hour"))
      else if (toMinutes / 60 > 1)
        Some((toMinutes / 60, "hours"))
      else
        None

    hoursData match
      case Some(value) =>
        s"${value._1} ${value._2} ${toMinutes % 60} minutes"
      case None =>
        s"${toMinutes % 60} minutes"
  }

  def times(
    int: Int,
  ) = MinuteDuration.ofMinutes(toMinutes.toInt * int)

  def dividedBy(
    duration: MinuteDuration,
  ) = toMinutes / duration.toMinutes

  def canEqual(
    other: Any,
  ): Boolean = other.isInstanceOf[MinuteDuration]

  override def equals(
    other: Any,
  ): Boolean =
    other match {
      case that: MinuteDuration =>
        (that.canEqual(this)) &&
        toMinutes == that.toMinutes
      case _ => false
    }

  override def hashCode(): Int = {
    val state = Seq(toMinutes)
    state
      .map(_.hashCode())
      .foldLeft(0)(
        (
          a,
          b,
        ) => 31 * a + b,
      )
  }
}

object MinuteDuration {

  def between(
    a: WallTime,
    b: WallTime,
  ) =
    MinuteDuration(
      Minutes.safe(
        math.abs(a.localTime.value - b.localTime.value).toInt,
      ),
    )

  def ofMinutes(
    minutes: Int,
  ) = new MinuteDuration(Minutes.safe(minutes))

  class DurationFriendlyInt(
    int: Int) {
    def minutes: MinuteDuration = MinuteDuration.ofMinutes(int)
  }

  implicit def toMinuteDuration(
    int: Int,
  ): DurationFriendlyInt = new DurationFriendlyInt(int)
}
