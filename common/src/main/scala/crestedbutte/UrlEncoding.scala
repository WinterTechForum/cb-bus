package crestedbutte

object UrlEncoding {
  import zio.json.*

  import java.net.{URLDecoder, URLEncoder}
  import java.nio.charset.StandardCharsets

  val urlEncoder = java.util.Base64.getUrlEncoder
  val urlDecoder = java.util.Base64.getUrlDecoder

  def encode(
    plan: Plan,
  ): String = urlEncoder.encodeToString(plan.toJson.getBytes())

  def decodePlan(
    raw: String,
  ) = {
    val rawJson = String(urlDecoder.decode(raw))
    val res =
      rawJson
        .fromJson[Plan]
    println("saved plan")
    res
  }

}
