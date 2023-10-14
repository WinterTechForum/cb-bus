package crestedbutte

import com.billding.time.{TimePicker, WallTime}
import crestedbutte.Browser.Browser
import crestedbutte.laminar.{AppMode, Components}
import org.scalajs.dom
import org.scalajs.dom.experimental.serviceworkers._
import urldsl.errors.DummyError
import urldsl.language.QueryParameters
import zio.{ZIO, ZLayer}

import java.time.{OffsetDateTime, ZoneId}
import scala.util.{Failure, Success}
import zio.ZIOAppDefault

object MyApp extends ZIOAppDefault {
  TimePicker

  override def run = {
    val myEnvironment =
      ZLayer.succeed(BrowserLive.browser)
    fullApplicationLogic.provide(myEnvironment)
  }

  val fullApplicationLogic =
    for {
      _ <- registerServiceWorker()
      _ <- ZIO.attempt {
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
      .service[Browser]
      .map { browser =>
        // TODO Ew. Try to get this removed after first version of PWA is working
        import scala.concurrent.ExecutionContext.Implicits.global

//         TODO Any good way to wield this?
        val serviceWorker = toServiceWorkerNavigator(
          browser.window().navigator,
        ).serviceWorker

        println("Deleting old serviceWorkers")

        serviceWorker
          .register("./sw-opt.js")
          .toFuture
          .onComplete {
            case Success(registration) =>
              println("Registered SW: " + registration.scope)
              registration.update()
            case Failure(error) =>
              println(
                s"registerServiceWorker: service worker registration failed > ${error.printStackTrace()}",
              )
          }

//        toServiceWorkerNavigator(
//          browser.window().navigator,
//        ).serviceWorker
//          .register("./sw-opt.js")
//          .toFuture
//          .onComplete {
//            case Success(registration) =>
//              println("Registered SW: " + registration.scope)
//              registration.update()
//            case Failure(error) =>
//              println(
//                s"registerServiceWorker: service worker registration failed > ${error.printStackTrace()}",
//              )
//          }

      }

}

object RoutingStuff {
  import com.raquo.laminar.api.L._

  import com.raquo.laminar.api.L
  import com.raquo.waypoint._
  import upickle.default._

  private case class BusPage(
    mode: AppMode,
    time: Option[WallTime], // TODO Make this a WallTime instead
    component: Option[ComponentName]) {

    val fixedTime = time

    val javaClock =
      if (fixedTime.isDefined)
        java.time.Clock.fixed(
          OffsetDateTime
            .parse(
              s"2020-02-21T${fixedTime.get.toEUString}:00.00-07:00",
            )
            .toInstant,
          ZoneId.of("America/Denver"),
        )
      else
        java.time.Clock.system(ZoneId.of("America/Denver"))
  }

  implicit val wallTimeRw: ReadWriter[WallTime] =
    readwriter[String].bimap[WallTime](_.toEUString, WallTime(_))

  implicit private val componentNameRw: ReadWriter[ComponentName] =
    macroRW
  implicit private val rw: ReadWriter[BusPage] = macroRW

  private val encodePage
    : BusPage => (Option[String], Option[String], Option[String]) =
    page =>
      (Some(page.mode.toString),
       page.time.map(_.toEUString),
       page.component.map(_.name),
      )

  private val decodePage: (
    (Option[String], Option[String], Option[String]),
  ) => BusPage = { case (mode, time, component) =>
    BusPage(
      mode = mode.map(AppMode.withName).getOrElse(AppMode.Production),
      time = time.map(WallTime.apply),
      component = component.map(ComponentName.apply),
    )
  }

  val params: QueryParameters[
    (Option[String], Option[String], Option[String]),
    DummyError,
  ] =
    param[
      String,
    ]("mode").? & param[String]("time").? & param[String](
      "component",
    ).?

  private val devRoute =
    Route.onlyQuery[BusPage,
                    (Option[String], Option[String], Option[String]),
    ](
      encode = encodePage,
      decode = decodePage,
      pattern = (root / "index_dev.html" / endOfSegments) ? params,
    )

  private val prodRoute =
    Route.onlyQuery[BusPage,
                    (Option[String], Option[String], Option[String]),
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
        rw,
      ), // serialize page data for storage in History API log
    deserializePage =
      pageStr => read(pageStr)(rw), // deserialize the above
    routeFallback = _ =>
      BusPage(
        mode = AppMode.Production,
        time = None, // TODO Make this a WallTime instead
        component = None,
      ),
  )(
    popStateEvents = L.windowEvents(
      _.onPopState,
    ), // this is how Waypoint avoids an explicit dependency on Laminar
    owner =
      L.unsafeWindowOwner, // this router will live as long as the window
  )

  private def renderMyPage(
    $loginPage: Signal[BusPage],
  ) =
    div(
      child <-- $loginPage.map(busPageInfo =>
        // TODO Start pulling out route queryParam
        Components.FullApp(busPageInfo.mode,
                           busPageInfo.component,
                           busPageInfo.javaClock,
        ),
      ),
    )

  private val splitter =
    SplitRender[BusPage, HtmlElement](router.currentPageSignal)
      .collectSignal[BusPage](renderMyPage)

  val app: Div = div(
    child <-- splitter.signal,
  )

}
