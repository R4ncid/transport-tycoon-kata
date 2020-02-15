package eu.ddd.transportTycoon

trait Logger {
  def log(event: Event):Unit
}

case class ConsoleLogger(eventEmitter: EventEmitter) extends Logger{

  val events = List(Events.start, Events.oneHourPassed, Events.cargoDelivered, Events.cargoPicked)
  events.foreach(e => eventEmitter.listen(e, log))


  override def log(event: Event): Unit = {
    event match {
      case event: Event => println(event)
      case _ =>
    }
  }
}