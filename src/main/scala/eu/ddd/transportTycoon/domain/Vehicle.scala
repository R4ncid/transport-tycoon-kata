package eu.ddd.transportTycoon.domain

import eu.ddd.transportTycoon.EventEmitter
import eu.ddd.transportTycoon.domain._

sealed trait Vehicle {
  protected var cargo: Option[Cargo] = None
  protected var time: Int = 0
  def id:Int
  def kind: String = this match {
    case t: Truck => "TRUCK"
    case s: Ship => "SHIP"
  }
  def deliverCargo(eventEmitter: EventEmitter): Unit = route().location match {
    case warehouse: Warehouse =>
      warehouse.store(cargo.get)
      eventEmitter.emit(CargoDelivered(cargo.get, this, warehouse, time))
      cargo = None
  }

  def pickCargo(eventEmitter: EventEmitter): Unit = route().location match {
    case producer: Producer =>
      val newCargo = producer.pick
      cargo = newCargo
      if (newCargo.isDefined) {
        eventEmitter.emit(CargoPicked(newCargo.get, this, producer, time))
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

  override def handleCargo(): Unit = {
    status match {
      case Running(route) =>
        eventEmitter.emit(Arrived(cargo, this, time, route))
      case _ =>
    }
    route().location match {
      case Factory => pickCargo(eventEmitter)
      case _: Warehouse => deliverCargo(eventEmitter)
    }
  }

  override def updateRoute(): Unit = {
    status = route().location match {
      case Port =>
        eventEmitter.emit(Departed(cargo, this, time, Routes.fromFactoryToPort))
        Running(Routes.fromPortToFactory)
      case B =>
        eventEmitter.emit(Departed(cargo, this, time, Routes.fromFactoryToB))
        Running(Routes.fromBToFactory)
      case Factory =>
        cargo match {
          case Some(Cargo(_, A)) =>
            eventEmitter.emit(Departed(cargo, this, time, Routes.fromPortToFactory))
            Running(Routes.fromFactoryToPort)
          case Some(Cargo(_, B)) =>
            eventEmitter.emit(Departed(cargo, this, time, Routes.fromBToFactory))
            Running(Routes.fromFactoryToB)
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

  override def handleCargo(): Unit = {
    status match {
      case Running(route) =>
        eventEmitter.emit(Arrived(cargo, this, time, route))
      case _ =>
    }
    route().location match {
      case Port => pickCargo(eventEmitter)
      case A => deliverCargo(eventEmitter)
    }
  }

  override def updateRoute(): Unit =
    status = route().location match {
      case A =>
        eventEmitter.emit(Departed(cargo, this, time, Routes.fromPortToA))
        Running(Routes.fromAToPort)
      case Port =>
        cargo match {
          case Some(Cargo(_, A)) =>
            eventEmitter.emit(Departed(cargo, this, time, Routes.fromAToPort))
            Running(Routes.fromPortToA)
          case None => status
        }
    }
}

sealed trait VehicleStatus

case class Waiting(location: Location) extends VehicleStatus
case class Running(route: Route) extends VehicleStatus
