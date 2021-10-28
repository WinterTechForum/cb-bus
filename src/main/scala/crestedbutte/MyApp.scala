package crestedbutte

import com.billding.time.{TimePicker, WallTime}
import crestedbutte.Browser.Browser
import crestedbutte.laminar.{AppMode, TagsOnlyLocal}
import org.scalajs.dom
import org.scalajs.dom.experimental.serviceworkers._
import urldsl.errors.DummyError
import urldsl.language.QueryParameters
import zio.clock._
import zio.console.Console
import zio.{App, ZIO, ZLayer}

import java.time.{OffsetDateTime, ZoneId}
import scala.util.{Failure, Success}

object MyApp extends App {
  TimePicker

  override def run(
    args: List[String],
  ): ZIO[zio.ZEnv, Nothing, zio.ExitCode] = {
    val myEnvironment =
      ZLayer.succeed(BrowserLive.browser) ++ Console.live

    fullApplicationLogic.provideLayer(myEnvironment).exitCode
  }

  val fullApplicationLogic =
    for {
      _ <- registerServiceWorker()
      _ <- ZIO {
        val appHolder = dom.document.getElementById("landing-message")
        appHolder.innerHTML = ""
        com.raquo.laminar.api.L.render(
          appHolder,
          RoutingStuff.app,
        )
      }
    } yield 0

  def registerServiceWorker(): ZIO[Browser, Nothing, Unit] =
    ZIO
      .access[Browser](_.get)
      .map {
        browser =>
          // TODO Ew. Try to get this removed after first version of PWA is working
          import scala.concurrent.ExecutionContext.Implicits.global
          println("Attempting to register sw")

          toServiceWorkerNavigator(browser.window().navigator).serviceWorker
            .register("./sw-opt.js")
            .toFuture
            .onComplete {
              case Success(registration) =>
                registration.update()
              case Failure(error) =>
                println(
                  s"registerServiceWorker: service worker registration failed > ${error.printStackTrace()}",
                )
            }
      }

}

object RoutingStuff {
  import com.raquo.laminar.api.L._

  import com.raquo.laminar.api.L
  import com.raquo.waypoint._
  import upickle.default._

  sealed private trait Page

  private case class BusPage(
    mode: String,
    time: Option[String], // TODO Make this a WallTime instead
    route: Option[String],
  ) extends Page {

    val fixedTime = time.map(WallTime(_))

    val javaClock =
      if (fixedTime.isDefined)
        java.time.Clock.fixed(
          OffsetDateTime
            .parse(
              s"2020-02-21T${fixedTime.get.toString}:00.00-07:00",
            )
            .toInstant,
          ZoneId.of("America/Denver"),
        )
      else
        java.time.Clock.system(ZoneId.of("America/Denver"))
  }

  private case object LoginPageOriginal extends Page

  implicit private val AppModeRW: ReadWriter[AppMode] = macroRW
  implicit private val BusPageRW: ReadWriter[BusPage] = macroRW
  implicit private val rw: ReadWriter[Page] = macroRW

  private val encodePage
    : BusPage => (Option[String], Option[String], Option[String]) =
    page => (Some(page.mode), page.time, page.route)

  private val decodePage: (
    (Option[String], Option[String], Option[String]),
  ) => BusPage = {
    case (mode, time, route) =>
      BusPage(
        mode = mode.getOrElse(AppMode.Production.toString),
        time = time,
        route = route,
      )
  }

  val params: QueryParameters[
    (Option[String], Option[String], Option[String]),
    DummyError,
  ] =
    param[
      String,
    ]("mode").? & param[String]("time").? & param[String]("route").?
  println("Get params")

  private val devRoute =
    Route.onlyQuery[BusPage,
                    (Option[String], Option[String], Option[String])](
      encode = encodePage,
      decode = decodePage,
      pattern = (root / "index_dev.html" / endOfSegments) ? params,
    )

  private val prodRoute =
    Route.onlyQuery[BusPage,
                    (Option[String], Option[String], Option[String])](
      encode = encodePage,
      decode = decodePage,
      pattern = (root / endOfSegments) ? params,
    )

  println("Creating router")

  private val router = new Router[Page](
    routes = List(
      prodRoute,
//      devRoute,
    ),
    getPageTitle = _.toString, // mock page title (displayed in the browser tab next to favicon)
    serializePage = page => write(page)(rw), // serialize page data for storage in History API log
    deserializePage = pageStr => read(pageStr)(rw), // deserialize the above
    routeFallback = _ =>
      BusPage(
        mode = "Production",
        time = None, // TODO Make this a WallTime instead
        route = None,
      ),
  )(
    $popStateEvent = L.windowEvents.onPopState, // this is how Waypoint avoids an explicit dependency on Laminar
    owner = L.unsafeWindowOwner, // this router will live as long as the window
  )
  println("Created router")

  private def renderMyPage(
    $loginPage: Signal[BusPage],
  ) =
    div(
      child <-- $loginPage.map(
        busPageInfo =>
          // TODO Start pulling out route queryParam
          TagsOnlyLocal.FullApp(AppMode.withName(busPageInfo.mode),
                                busPageInfo.route,
                                busPageInfo.javaClock),
      ),
    )

  private val splitter =
    SplitRender[Page, HtmlElement](router.$currentPage)
      .collectSignal[BusPage](renderMyPage)
      .collectStatic(LoginPageOriginal) { div("Login page") }

  val app: Div = div(
    child <-- splitter.$view,
  )

}
