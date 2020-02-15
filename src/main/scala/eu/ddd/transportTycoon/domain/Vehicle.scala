package eu.ddd.transportTycoon.domain

import eu.ddd.transportTycoon.{CargoDelivered, CargoPicked, Event, EventEmitter, Events, OneHourPassed}

sealed trait Vehicle {
  protected var cargo: Option[Cargo] = None
  protected var time: Int = 0
  def deliverCargo(eventEmitter: EventEmitter): Unit = route().location match {
    case warehouse: Warehouse =>
      warehouse.store(cargo.get)
      eventEmitter.fire(CargoDelivered(cargo.get, this, warehouse, time))
      cargo = None
  }

  def pickCargo(eventEmitter: EventEmitter): Unit = route().location match {
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


case class Truck(id: Int, private var status: VehicleStatus, eventEmitter: EventEmitter) extends Vehicle {

  eventEmitter.listen(Events.start, update)
  eventEmitter.listen(Events.oneHourPassed, update)

  def move(): Unit = status match {
    case Waiting(_) =>
    case Running(route) => status = Running(route.proceed)
  }

  override def route(): Route = status match {
    case Waiting(location: Location) => NoRoute(location)
    case Running(route) => route
  }

  override def handleCargo(): Unit = route().location match {
    case Factory => pickCargo(eventEmitter)
    case _: Warehouse => deliverCargo(eventEmitter)
  }

  override def updateRoute(): Unit = {
    status = route().location match {
      case Port => Running(Routes.fromPortToFactory)
      case B => Running(Routes.fromBToFactory)
      case Factory =>
        cargo match {
          case Some(Cargo(_, A)) => Running(Routes.fromFactoryToPort)
          case Some(Cargo(_, B)) => Running(Routes.fromFactoryToB)
          case None => status
        }
    }
  }


}

case class Ship(id: Int, private var status:VehicleStatus, eventEmitter: EventEmitter) extends Vehicle {

  eventEmitter.listen(Events.start, update)
  eventEmitter.listen(Events.oneHourPassed, update)

  def move(): Unit = status match {
    case Waiting(_) =>
    case Running(route) => status = Running(route.proceed)
  }

  override def route(): Route = status match {
    case Waiting(location) => NoRoute(location)
    case Running(route) => route
  }

  override def handleCargo(): Unit = route().location match {
    case Port => pickCargo(eventEmitter)
    case A => deliverCargo(eventEmitter)
  }

  override def updateRoute(): Unit =
    status = route().location match {
      case A => Running(Routes.fromAToPort)
      case Port =>
        cargo match {
          case Some(Cargo(_, A)) => Running(Routes.fromPortToA)
          case None => status
        }
    }
}

sealed trait VehicleStatus

case class Waiting(location: Location) extends VehicleStatus
case class Running(route: Route) extends VehicleStatus
