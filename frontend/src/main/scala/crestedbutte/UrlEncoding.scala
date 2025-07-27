package crestedbutte

import crestedbutte.pwa.Persistence

object UrlEncoding {
  import zio.json.*

  import java.net.{URLDecoder, URLEncoder}
  import java.nio.charset.StandardCharsets

  val urlEncoder = java.util.Base64.getUrlEncoder
  val urlDecoder = java.util.Base64.getUrlDecoder

  def encode(
    plan: Plan,
  ): String =
    println(plan.toJson)
    urlEncoder.encodeToString(plan.toJson.getBytes())

  def decodePlan(
    raw: String,
  ) = {
    val rawJson = String(urlDecoder.decode(raw))
    val res =
      rawJson
        .fromJson[Plan]
    try
      Persistence().saveDailyPlanOnly(res.getOrElse(???))
    catch {
      case ex => println("Probably not running in a browser")
    }
    println("saved plan")
    res
  }

}
