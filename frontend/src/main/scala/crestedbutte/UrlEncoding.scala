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
//    val base64 =
    urlEncoder.encodeToString(plan.toJson.getBytes())
//    URLEncoder.encode(base64, StandardCharsets.UTF_8.toString);

  def decode(
    raw: String,
  ) = {
//    val base64 = URLDecoder
//      .decode(raw, StandardCharsets.UTF_8.toString)
    val rawJson = String(urlDecoder.decode(raw))
    println("rawJson: " + rawJson)
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
