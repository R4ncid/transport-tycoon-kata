package eu.ddd.transportTycoon.domain


sealed trait Location


trait FinalDestination

trait Warehouse extends Location {
  def store(cargo: Cargo): Unit
}

trait Producer extends Location {
  def pick: Option[Cargo]
}


case object Factory extends Producer {
  private var cargoes: List[Cargo] = List()

  def init(list: List[Cargo]):Unit = cargoes = list

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
  override def store(cargo: Cargo): Unit = {

  }
}

case object B extends Warehouse with FinalDestination {
  override def store(cargo: Cargo): Unit = {}
}
