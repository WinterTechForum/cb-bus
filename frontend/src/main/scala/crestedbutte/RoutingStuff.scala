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

  val params: QueryParameters[
    (Option[String], Option[String], Option[String]),
    DummyError,
  ] =
    param[
      String,
    ]("mode").? & param[String]("time").? & param[String]("plan").?

  private val prodRoute =
    Route.onlyQuery[BusPage,
                    (
                      Option[String],
                      Option[String],
                      Option[String],
                    ),
    ](
      encode = BusPage.encodePage,
      decode = BusPage.decodePage,
      pattern = root ? params,
    )

  private val router = new Router[BusPage](
    routes = List(
      prodRoute,
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
        fixedTime = None,
        plan = None,
      ),
  )(
    popStateEvents = L.windowEvents(
      _.onPopState,
    ), // Lets Waypoint avoids an explicit dependency on Laminar
    owner =
      L.unsafeWindowOwner, // this router will live as long as the window
  )

  private val splitter =
    SplitRender[BusPage, HtmlElement](router.currentPageSignal)
      .collectSignal[BusPage]($loginPage =>
        div(
          child <-- $loginPage.map(busPageInfo =>
            Components.FullApp(
              busPageInfo.javaClock,
            ),
          ),
        ),
      )

  val app: Div = div(
    child <-- splitter.signal,
  )

}
