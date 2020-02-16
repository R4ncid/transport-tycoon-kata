package eu.ddd.transportTycoon.domain


object Events {
  def cargoDelivered: EventName = EventName("CargoDelivered")
  def cargoPicked: EventName = EventName("CargoPicked")
  def oneHourPassed: EventName = EventName("OneHourPassed")
  def start: EventName = EventName("Start")
  def departed: EventName = EventName("Departed")
  def arrived: EventName = EventName("Arrived")
}

case class EventName(name: String)

trait Event{
  def name: EventName
}

case class Departed(cargo: Option[Cargo], vehicle: Vehicle, time: Int, route: Route) extends Event{
  override def name: EventName = Events.departed
}
case class Arrived(cargo: Option[Cargo], vehicle: Vehicle, time: Int, route: Route) extends Event{
  override def name: EventName = Events.arrived
}

case class CargoDelivered(cargo: Cargo, vehicle: Vehicle, warehouse: Warehouse, time: Int) extends Event{
  override def name: EventName = Events.cargoDelivered
}

case class CargoPicked(cargo: Cargo,vehicle: Vehicle, producer: Producer, time: Int) extends Event{
  override def name: EventName = Events.cargoPicked
}

case object Start extends Event {
  override def name: EventName = Events.start
}

case object CargoMade extends Event {
  override def name: EventName = EventName("CargoMade")
}

case class OneHourPassed(time:Int) extends Event {
    override def name: EventName = Events.oneHourPassed
}