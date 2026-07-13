package crestedbutte.e2e

import com.sun.net.httpserver.{HttpExchange, HttpHandler, HttpServer}

import java.io.File
import java.net.InetSocketAddress
import java.nio.file.{Files, Path, Paths}

/** Minimal static file server backed by the JDK's built-in HttpServer.
  *
  * Serves the app's `frontend/src/main/resources` directory so the e2e suite is
  * self-contained (no external `python3 -m http.server` to manage). Roots at
  * `/` because the app references some assets with absolute paths.
  */
final class StaticServer private (server: HttpServer, val port: Int):
  def baseUrl: String = s"http://localhost:$port"
  def stop(): Unit    = server.stop(0)

object StaticServer:

  private val contentTypes: Map[String, String] = Map(
    "html"        -> "text/html; charset=utf-8",
    "js"          -> "application/javascript; charset=utf-8",
    "map"         -> "application/json; charset=utf-8",
    "css"         -> "text/css; charset=utf-8",
    "json"        -> "application/json; charset=utf-8",
    "webmanifest" -> "application/manifest+json; charset=utf-8",
    "ico"         -> "image/x-icon",
    "png"         -> "image/png",
    "jpg"         -> "image/jpeg",
    "svg"         -> "image/svg+xml",
    "woff"        -> "font/woff",
    "woff2"       -> "font/woff2",
    "txt"         -> "text/plain; charset=utf-8",
  )

  private def contentTypeFor(path: Path): String =
    val name = path.getFileName.toString
    val ext  = name.lastIndexOf('.') match
      case -1 => ""
      case i  => name.substring(i + 1).toLowerCase
    contentTypes.getOrElse(ext, "application/octet-stream")

  /** Start on an ephemeral port (0). Root must be an existing directory. */
  def start(root: File): StaticServer =
    require(root.isDirectory, s"resource root does not exist: $root")
    val rootPath = root.toPath.toRealPath()
    val server   = HttpServer.create(new InetSocketAddress("localhost", 0), 0)

    server.createContext(
      "/",
      new HttpHandler:
        def handle(exchange: HttpExchange): Unit =
          try
            var rel = exchange.getRequestURI.getPath.stripPrefix("/")
            if rel.isEmpty then rel = "index.html"
            // Resolve and guard against path traversal escaping the root.
            val resolved = rootPath.resolve(rel).normalize()
            if !resolved.startsWith(rootPath) then
              exchange.sendResponseHeaders(403, -1)
            else
              val target =
                if Files.isDirectory(resolved) then resolved.resolve("index.html")
                else resolved
              if Files.isRegularFile(target) then
                val bytes = Files.readAllBytes(target)
                exchange.getResponseHeaders.set("Content-Type", contentTypeFor(target))
                exchange.sendResponseHeaders(200, bytes.length.toLong)
                val os = exchange.getResponseBody
                try os.write(bytes)
                finally os.close()
              else exchange.sendResponseHeaders(404, -1)
          catch
            case _: Throwable =>
              try exchange.sendResponseHeaders(500, -1)
              catch case _: Throwable => ()
          finally exchange.close(),
    )

    server.setExecutor(null)
    server.start()
    new StaticServer(server, server.getAddress.getPort)
