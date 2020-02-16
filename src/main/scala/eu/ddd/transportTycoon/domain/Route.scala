package eu.ddd.transportTycoon.domain

trait Route{
  def isArrived:Boolean
  def proceed: Route
  def location:Location
}

case class RouteWithDestination (from: Location, to: Location, time: Int) extends Route {

  def isArrived: Boolean = time < 1

  def proceed : Route = this.copy(time = Math.max(time - 1, 0))

  override def location: Location = to
}

case class NoRoute(location: Location) extends Route {
  override def isArrived: Boolean = true

  override def proceed: Route = this

}


object Routes{
  def fromFactoryToB: Route = RouteWithDestination(Factory, B, 5)
  def fromFactoryToPort: Route = RouteWithDestination(Factory, Port, 1)
  def fromBToFactory: Route = RouteWithDestination(B, Factory, 5)
  def fromPortToFactory: Route = RouteWithDestination(Port, Factory, 1)
  def fromPortToA: Route = RouteWithDestination(Port, A, 4)
  def fromAToPort: Route = RouteWithDestination(A, Port, 4)
}
