package crestedbutte

import zio.ZIO
import crestedbutte.Browser.Browser

object DomMonitoring {

  val modalIsOpen: ZIO[Browser, Nothing, Boolean] =
    ZIO
      .service[Browser]
      .map { browser =>
        browser
          .body()
          .querySelectorAll(".modal.is-active") // LONG SEARCH
          .length > 0
      }

}
