package crestedbutte

object AppMode extends Enumeration {

  protected case class Val(
    name: String)
      extends super.Val(name)

  import scala.language.implicitConversions

  implicit def valueToStopLocationVal(
    x: Value,
  ): Val =
    x.asInstanceOf[Val]

  type StopLocation = Value

  val Production: Val = Val("production")
  val Development: Val = Val("dev")

  def fromString(
    input: String,
  ) =
    values.find(_.name == input)
}

/*
Hey Jeremy,
Thanks for getting back to me. Sounds like I contacted you just a bit too late.
Understanding that the Mountain Express won't be taking action on this in the near-term,
I think it would still be worthwhile to have a quick 20 minute chat about this when schedules allow.

This situation is a bit unusual in that I've already built up the basic application to suit my needs as a regular, local bus rider.
I can quickly show you what's available currently.

On a different note, is the Mountain Express currently active in the Transit App?
I downloaded it and scanned the area, and I'm only getting results for RTA buses.

 */
