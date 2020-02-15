package eu.ddd.transportTycoon

import eu.ddd.transportTycoon.domain.{Cargo, Producer, Vehicle, Warehouse}

case class EventName(name: String)
trait Event{
  def name: EventName
}

case class EventEmitter() {

  type Listener = Event => Unit

  var listeners: Map[EventName, List[Listener]] = Map()

  def listen(eventName: EventName, listener: Listener): Unit = {
    val actualListener = listeners.getOrElse(eventName, List())
    listeners = listeners + (eventName -> (actualListener ::: List(listener)))
  }

  def fire(event: Event): Unit =
  listeners.getOrElse(event.name, List()).foreach(l => l(event))
}



object Events {
  def cargoDelivered: EventName = EventName("CargoDelivered")
  def cargoPicked: EventName = EventName("CargoPicked")
  def oneHourPassed: EventName = EventName("OneHourPassed")
  def start: EventName = EventName("Start")
  def departed: EventName = EventName("Departed")
  def arrived: EventName = EventName("Arrived")
}

case class Departed(cargo: Cargo, vehicle: Vehicle, time: Int) extends Event{
  override def name: EventName = Events.departed
}
case class Arrived(cargo: Cargo, vehicle: Vehicle, time: Int) extends Event{
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

case class OneHourPassed(time:Int) extends Event {
    override def name: EventName = Events.oneHourPassed
}