package eu.ddd.transportTycoon

import eu.ddd.transportTycoon.domain._
import eu.ddd.transportTycoon.infrastructure.FileLogWriter

object TransportTycoon {

  def calculateTime(cargoesList: String): Int = {

    val eventEmitter = EventEmitter()

    val counter = TimeCounter(eventEmitter)
    ConsoleLogger(eventEmitter, List(Events.start, Events.oneHourPassed, Events.cargoPicked, Events.cargoDelivered))
    FileLogger(FileLogWriter(cargoesList), eventEmitter, List(Events.departed, Events.arrived, Events.start))
    Truck(0, Waiting(Factory), eventEmitter)
    Truck(1, Waiting(Factory), eventEmitter)
    Ship(2, Waiting(Port), eventEmitter)

    Factory.produce(cargoesList, eventEmitter)

    counter.count
  }
}
