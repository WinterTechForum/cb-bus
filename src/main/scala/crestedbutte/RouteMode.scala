package crestedbutte

enum RouteMode(
  name: String) {
  // TODO Check ordering of all coordinates
  case Active extends RouteMode("Active")
  case Hidden extends RouteMode("Hidden")
}
