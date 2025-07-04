package crestedbutte

import com.billding.time.WallTime
import crestedbutte.laminar.{AppMode, Components}
import crestedbutte.url_routing.BusPage
import urldsl.errors.DummyError
import urldsl.language.QueryParameters

import java.time.OffsetDateTime

object RoutingStuff {
  import com.raquo.laminar.api.L
  import com.raquo.laminar.api.L.*
  import com.raquo.waypoint.*
  import upickle.default.*

  private val encodePage: BusPage => (
    Option[String],
    Option[String],
    Option[String],
  ) =
    page =>
      (Some(page.mode.toString),
       page.time.map(_.toEUString),
       page.plan.map(UrlEncoding.encode),
      )

  private val decodePage = {
    (
      mode: Option[String],
      time: Option[String],
      plan: Option[String],
    ) =>
      BusPage(
        mode =
          mode.map(AppMode.valueOf).getOrElse(AppMode.Production),
        time = time.map(WallTime.apply),
        plan = plan.flatMap(UrlEncoding.decode(_).toOption),
      )
  }.tupled

  val params: QueryParameters[
    (Option[String], Option[String], Option[String]),
    DummyError,
  ] =
    param[
      String,
    ]("mode").? & param[String]("time").? & param[String]("plan").?

  private val devRoute =
    Route.onlyQuery[BusPage,
                    (
                      Option[String],
                      Option[String],
                      Option[String],
                    ),
    ](
      encode = encodePage,
      decode = decodePage,
      pattern = (root / "index_dev.html" / endOfSegments) ? params,
    )

  private val prodRoute =
    Route.onlyQuery[BusPage,
                    (
                      Option[String],
                      Option[String],
                      Option[String],
                    ),
    ](
      encode = encodePage,
      decode = decodePage,
      pattern = (root / endOfSegments) ? params,
    )

  private val router = new Router[BusPage](
    routes = List(
      prodRoute,
      devRoute,
    ),
    getPageTitle =
      _.toString, // mock page title (displayed in the browser tab next to favicon)
    serializePage = page =>
      write(page)(
        BusPage.rw,
      ), // serialize page data for storage in History API log
    deserializePage =
      pageStr => read(pageStr)(BusPage.rw), // deserialize the above
    routeFallback = _ =>
      BusPage(
        mode = AppMode.Production,
        time = None,
        plan = None,
      ),
  )(
    popStateEvents = L.windowEvents(
      _.onPopState,
    ), // this is how Waypoint avoids an explicit dependency on Laminar
    owner =
      L.unsafeWindowOwner, // this router will live as long as the window
  )

  private val splitter =
    SplitRender[BusPage, HtmlElement](router.currentPageSignal)
      .collectSignal[BusPage]($loginPage =>
        div(
          child <-- $loginPage.map(busPageInfo =>
            // TODO Start pulling out route queryParam
            Components.FullApp(busPageInfo.mode,
                               busPageInfo.javaClock,
            ),
          ),
        ),
      )

  val app: Div = div(
    child <-- splitter.signal,
  )

}
