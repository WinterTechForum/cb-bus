package crestedbutte

sealed trait ComponentData {
//  namedRoute: NamedRoute,

  val componentName: RouteName
}

case class ComponentDataTyped[T](
  value: T,
  componentName: RouteName)
    extends ComponentData

case class ComponentDataRoute(
  namedRoute: NamedRoute,
) extends ComponentData {
  val componentName = namedRoute.routeName
}
