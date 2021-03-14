package crestedbutte

import com.billding.time.{BusTime, ColoradoClock}
import crestedbutte.Browser.Browser
import crestedbutte.laminar.AppMode
import crestedbutte.laminar._
import org.scalajs.dom
import org.scalajs.dom.experimental.serviceworkers._
import zio.clock._
import zio.console.Console
import zio.{App, ZIO, ZLayer}

import java.time.{OffsetDateTime, ZoneId}
import scala.util.{Failure, Success}

object MyApp extends App {

  override def run(
    args: List[String],
  ): ZIO[zio.ZEnv, Nothing, zio.ExitCode] = {
    val myEnvironment =
      ZLayer.succeed(BrowserLive.browser) ++ Console.live ++
      ZLayer.succeed(ColoradoClock.live)

    import com.raquo.waypoint.root
    println("root: " + root.createPath())
    fullApplicationLogic.provideLayer(myEnvironment).exitCode
  }

  val fullApplicationLogic =
    for {
      _ <- registerServiceWorker()
      _ <- ZIO {
        dom.document.getElementById("landing-message").innerHTML = ""
        com.raquo.laminar.api.L.render(
          dom.document.getElementById("landing-message"),
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
  import org.scalajs.dom

  sealed trait Page
  sealed trait AppPage extends Page

  sealed case class UserPage(
    userId: Int)
      extends AppPage

  sealed case class NotePage(
    workspaceId: Int,
    noteId: Int)
      extends AppPage

  case class BusPage(
    mode: String,
    time: Option[String], // TODO Make this a BusTime instead
    route: Option[String],
  ) extends Page {

    val fixedTime = time.map(BusTime(_))
    println("fixedTime: " + fixedTime)

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

  case object LoginPageOriginal extends Page

  val fancyPage = div("hi")

  implicit val AppModeRW: ReadWriter[AppMode] = macroRW
  implicit val BusPageRW: ReadWriter[BusPage] = macroRW
  implicit val NotePageRW: ReadWriter[NotePage] = macroRW
  implicit val UserPageRW: ReadWriter[UserPage] = macroRW
  implicit val AppPageRW: ReadWriter[AppPage] = macroRW
  implicit val rw: ReadWriter[Page] = macroRW

  val userRoute = Route(
    encode = (userPage: UserPage) => userPage.userId,
    decode = (arg: Int) => UserPage(userId = arg),
    pattern = root / "user" / segment[Int] / endOfSegments,
  )

  println("Root: " + root.createPath())

  import com.raquo.waypoint._

  // mode=dev&route=RoundTripCalculator&time=12:01
  val devRoute =
    Route.onlyQuery[BusPage,
                    (Option[String], Option[String], Option[String])](
      encode = page => (Some(page.mode), page.time, page.route),
      decode = {
        case (mode, time, route) =>
          BusPage(
            mode = mode.getOrElse(AppMode.Production.toString),
            time = time,
            route = route,
          )
      },
      pattern = (root / "index_dev.html" / endOfSegments) ? (param[
          String,
        ]("mode").? & param[String]("time").? & param[String]("route").?),
    )

  val prodRoute =
    Route.onlyQuery[BusPage,
                    (Option[String], Option[String], Option[String])](
      encode = page => (Some(page.mode), page.time, page.route),
      decode = {
        case (mode, time, route) =>
          BusPage(
            mode = mode.getOrElse(AppMode.Production.toString),
            time = time,
            route = route,
          )
      },
      pattern = (root / endOfSegments) ? (param[
          String,
        ]("mode").? & param[String]("time").? & param[String]("route").?),
    )

  val router = new Router[Page](
    routes = List(userRoute, prodRoute, devRoute),
    getPageTitle = _.toString, // mock page title (displayed in the browser tab next to favicon)
    serializePage = page => write(page)(rw), // serialize page data for storage in History API log
    deserializePage = pageStr => read(pageStr)(rw), // deserialize the above
//    routeFallback = _ => div("You are trying to access and invalid link")
  )(
    $popStateEvent = L.windowEvents.onPopState, // this is how Waypoint avoids an explicit dependency on Laminar
    owner = L.unsafeWindowOwner, // this router will live as long as the window
  )

  def renderMyPage(
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

  def renderUserPage(
    $userPage: Signal[UserPage],
  ): Div =
    div(
      "User page ",
      child.text <-- $userPage.map(user => user.userId),
    )

  val splitter = SplitRender[Page, HtmlElement](router.$currentPage)
    .collectSignal[AppPage] {
      $appPage =>
        renderAppPage($appPage)
    }
    .collectSignal[BusPage](renderMyPage)
    .collectStatic(LoginPageOriginal) { div("Login page") }

  val app: Div = div(
    child <-- splitter.$view,
  )

  def renderNotePage(
    $notePage: Signal[NotePage],
  ): Div =
    div(
      "Note page. workspaceid: ",
      child.text <-- $notePage.map(note => note.workspaceId),
      ", noteid: ",
      child.text <-- $notePage.map(note => note.noteId),
    )

  def renderAppPage(
    $appPage: Signal[AppPage],
  ): Div = {
    val appPageSplitter = SplitRender[AppPage, HtmlElement]($appPage)
      .collectSignal[UserPage] {
        $userPage =>
          renderUserPage($userPage)
      }
      .collectSignal[NotePage] {
        $notePage =>
          renderNotePage($notePage)
      }
    div(
      child <-- appPageSplitter.$view,
    )
  }

}
