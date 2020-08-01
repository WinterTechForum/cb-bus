package crestedbutte

import zio.{Has, ZIO}

object DomMonitoring {

  val modalIsOpen: ZIO[Has[Browser.Service], Nothing, Boolean] =
    ZIO
      .access[Has[Browser.Service]](_.get)
      .map {
        browser =>
          browser
            .body()
            .querySelectorAll(".modal.is-active") // LONG SEARCH
            .length > 0
      }

}
