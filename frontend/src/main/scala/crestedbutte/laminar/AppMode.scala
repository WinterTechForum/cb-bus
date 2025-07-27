package crestedbutte.laminar

import upickle.default._

enum AppMode derives ReadWriter:
  case Production, Local
