package eu.ddd.transportTycoon

import eu.ddd.transportTycoon.domain._

object TransportTycoon extends App {


  def calculateTime(cargoesList: String): Int = {

    val eventEmitter = EventEmitter()

    val timer = Timer(eventEmitter)
    ConsoleLogger(eventEmitter)
    Truck(1, Waiting(Factory), eventEmitter)
    Truck(2, Waiting(Factory), eventEmitter)
    Ship(3, Waiting(Port), eventEmitter)


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
