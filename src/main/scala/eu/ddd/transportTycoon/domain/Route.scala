package eu.ddd.transportTycoon.domain

case class Route (from: Location, to: Location, time: Int){

  def isArrived: Boolean = time < 1

  def proceed : Route = this.copy(time = Math.max(time - 1, 0))

}


object Routes{
  def fromFactoryToB: Route = Route(Factory, B, 5)
  def fromFactoryToPort: Route = Route(Factory, Port, 1)
  def fromBToFactory: Route = Route(B, Factory, 5)
  def fromPortToFactory: Route = Route(Port, Factory, 1)
  def fromPortToA: Route = Route(Port, A, 4)
  def fromAToPort: Route = Route(A, Port, 4)
}
