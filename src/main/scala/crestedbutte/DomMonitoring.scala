package crestedbutte

import zio.ZIO

object DomMonitoring {

  val modalIsOpen: ZIO[BrowserLive, Nothing, Boolean] =
    ZIO
      .environment[BrowserLive]
      .map { browser =>
        browser.browser
          .body()
          .querySelectorAll(".modal.is-active") // LONG SEARCH
          .length > 0
      }

}
