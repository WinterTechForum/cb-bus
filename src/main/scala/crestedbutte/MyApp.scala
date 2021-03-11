package crestedbutte

import com.billding.time.{BusTime, ColoradoClock, TurboClock}
import crestedbutte.Browser.Browser
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

    fullApplicationLogic.provideLayer(myEnvironment).exitCode
  }

  def getOptional[T](
    parameterName: String,
    typer: String => Option[T],
  ): ZIO[Browser, Nothing, Option[T]] =
    ZIO
      .access[Browser](_.get)
      .map(
        browser =>
          UrlParsing
            .getUrlParameter(
              browser.window().location.toString,
              parameterName,
            )
            .flatMap(typer),
      )

  val fullApplicationLogic =
    for {
      clockParam: Clock.Service <- ZIO.access[Clock](_.get)
      pageMode: AppMode.Value <- getOptional("mode",
                                             AppMode.fromString)
        .map(
          _.getOrElse(AppMode.Production),
        )
      fixedTime <- getOptional("time", x => Some(BusTime(x)))

      clock = if (fixedTime.isDefined)
        TurboClock.TurboClock(
          s"2020-02-21T${fixedTime.get.toString}:00.00-07:00",
        )
      else clockParam

      javaClock = if (fixedTime.isDefined)
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
      _ <- registerServiceWorker()
      _ <- ZIO {
        val initialRouteOpt: Option[String] =
          UrlParsing
            .getUrlParameter(
              dom.window.location.toString,
              "route",
            )

        dom.document.getElementById("landing-message").innerHTML = ""
        com.raquo.laminar.api.L.render(
          dom.document.getElementById("landing-message"),
          TagsOnlyLocal.FullApp(pageMode, initialRouteOpt, javaClock),
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
  case object LoginPage extends Page

  implicit val NotePageRW: ReadWriter[NotePage] = macroRW
  implicit val UserPageRW: ReadWriter[UserPage] = macroRW
  implicit val AppPageRW: ReadWriter[AppPage] = macroRW
  implicit val rw: ReadWriter[Page] = macroRW

  val userRoute = Route(
    encode = (userPage: UserPage) => userPage.userId,
    decode = (arg: Int) => UserPage(userId = arg),
    pattern = root / "user" / segment[Int] / endOfSegments,
  )

  val loginRoute =
    Route.static(LoginPage, root / "login" / endOfSegments)

  val router = new Router[Page](
    routes = List(userRoute, loginRoute),
    getPageTitle = _.toString, // mock page title (displayed in the browser tab next to favicon)
    serializePage = page => write(page)(rw), // serialize page data for storage in History API log
    deserializePage = pageStr => read(pageStr)(rw), // deserialize the above
  )(
    $popStateEvent = L.windowEvents.onPopState, // this is how Waypoint avoids an explicit dependency on Laminar
    owner = L.unsafeWindowOwner, // this router will live as long as the window
  )

  val pageSplitter =
    SplitRender[Page, HtmlElement](router.$currentPage)
      .collectSignal[AppPage] {
        $appPage =>
          renderAppPage($appPage)
      }
      .collectStatic(LoginPage) { div("Login page") }

  def renderUserPage(
    $userPage: Signal[UserPage],
  ): Div =
    div(
      "User page ",
      child.text <-- $userPage.map(user => user.userId),
    )

  val splitter = SplitRender[Page, HtmlElement](router.$currentPage)
    .collectSignal[UserPage] {
      $userPage =>
        renderUserPage($userPage)
    }
    .collectStatic(LoginPage) { div("Login page") }

  val app: Div = div(
    h1("Routing App"),
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
      h2("App header"),
      child <-- appPageSplitter.$view,
    )
  }

}
