package eu.ddd.transportTycoon

import eu.ddd.transportTycoon.domain._

object TransportTycoon extends App {


  def calculateTime(cargoesList: String): Int = {

    val eventEmitter = EventEmitter()

    val timer = Timer(eventEmitter)
    val logger = ConsoleLogger(eventEmitter)

    val vehicles = List(
      Truck(1, Route(Factory, Factory, 0), eventEmitter),
      Truck(2, Route(Factory, Factory, 0), eventEmitter),
      Ship(3, Route(Port, Port, 0), eventEmitter),
    )

    var id = 0
    val cargoes = cargoesList.split("") map {
      case "A" =>
        id = id + 1
        Cargo(id, A)
      case "B" =>
        id = id + 1
        Cargo(id, B)
    }
    Factory.init(cargoes.toList)

    timer.start(cargoes.length)
  }
}
