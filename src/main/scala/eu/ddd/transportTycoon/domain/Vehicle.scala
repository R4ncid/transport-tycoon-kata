package eu.ddd.transportTycoon.domain

import eu.ddd.transportTycoon.{CargoDelivered, CargoPicked, Event, EventEmitter, Events, OneHourPassed}

sealed trait Vehicle {
  protected var cargo: Option[Cargo] = None
  protected var time: Int = 0
  def deliverCargo(eventEmitter: EventEmitter): Unit = route().to match {
    case warehouse: Warehouse =>
      warehouse.store(cargo.get)
      eventEmitter.fire(CargoDelivered(cargo.get, this, warehouse, time))
      cargo = None
  }

  def pickCargo(eventEmitter: EventEmitter): Unit = route().to match {
    case producer: Producer =>
      val newCargo = producer.pick
      cargo = newCargo
      if (newCargo.isDefined) {
        eventEmitter.fire(CargoPicked(newCargo.get, this, producer, time))
      }
  }

  def handleCargo(): Unit

  def move(): Unit

  def route(): Route

  def updateRoute(): Unit

  def update(event:Event): Unit = {
    event match {
      case OneHourPassed(t) => time = t
      case _ =>
    }
    if (route().isArrived) {
      handleCargo()
      updateRoute()
    }
    move()
  }
}


case class Truck(id: Int, private var currentRoute: Route, eventEmitter: EventEmitter) extends Vehicle {

  eventEmitter.listen(Events.start, update)
  eventEmitter.listen(Events.oneHourPassed, update)

  def move(): Unit = currentRoute = currentRoute.proceed

  override def route(): Route = currentRoute

  override def handleCargo(): Unit = route().to match {
    case Factory => pickCargo(eventEmitter)
    case _: Warehouse => deliverCargo(eventEmitter)
  }

  override def updateRoute(): Unit = {
    currentRoute = route().to match {
      case Port => Routes.fromPortToFactory
      case B => Routes.fromBToFactory
      case Factory =>
        cargo match {
          case Some(Cargo(_, A)) => Routes.fromFactoryToPort
          case Some(Cargo(_, B)) => Routes.fromFactoryToB
          case None => Route(Factory, Factory, 0)
        }
    }
  }


}

case class Ship(id: Int, private var currentRoute: Route, eventEmitter: EventEmitter) extends Vehicle {

  eventEmitter.listen(Events.start, update)
  eventEmitter.listen(Events.oneHourPassed, update)

  def move(): Unit = currentRoute = currentRoute.proceed

  override def route(): Route = currentRoute

  override def handleCargo(): Unit = route().to match {
    case Port => pickCargo(eventEmitter)
    case A => deliverCargo(eventEmitter)
  }

  override def updateRoute(): Unit =
    currentRoute = route().to match {
      case A => Routes.fromAToPort
      case Port =>
        cargo match {
          case Some(Cargo(_, A)) => Routes.fromPortToA
          case None => Route(Port, Port, 0)
        }
    }
}