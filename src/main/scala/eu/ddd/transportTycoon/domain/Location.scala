package eu.ddd.transportTycoon.domain

import eu.ddd.transportTycoon.EventEmitter


sealed trait Location{
  override def toString: String = this match {
    case Factory => "FACTORY"
    case Port => "PORT"
    case A => "A"
    case B => "B"
  }
}


trait FinalDestination

trait Warehouse extends Location {
  def store(cargo: Cargo): Unit
}

trait Producer extends Location {
  def pick: Option[Cargo]
}


case object Factory extends Producer {
  def produce(cargoesList: String, eventEmitter: EventEmitter): Unit = {
    var id = 0
    cargoes = cargoesList.split("") map {
      case "A" =>
        id = id + 1
        eventEmitter.emit(CargoMade)
        Cargo(id, A)
      case "B" =>
        id = id + 1
        eventEmitter.emit(CargoMade)
        Cargo(id, B)
    } toList
  }


  private var cargoes: List[Cargo] = List()

  override def pick: Option[Cargo] =
    if (cargoes.isEmpty)
      None
    else {
      val cargo = cargoes.headOption
      cargoes = cargoes.tail
      cargo
    }
}


case object Port extends Producer with Warehouse {

  private var cargoes: List[Cargo] = List()

  override def pick: Option[Cargo] =
    if (cargoes.isEmpty)
      None
    else {
      val cargo = cargoes.headOption
      cargoes = cargoes.tail
      cargo
    }

  override def store(cargo: Cargo): Unit = cargoes = cargoes ::: List(cargo)
}

case object A extends Warehouse with FinalDestination {
  override def store(cargo: Cargo): Unit = ()
}

case object B extends Warehouse with FinalDestination {
  override def store(cargo: Cargo): Unit = ()
}
