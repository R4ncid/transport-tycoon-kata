package eu.ddd.transportTycoon

import eu.ddd.transportTycoon.domain._

class TimeCounter(eventEmitter: EventEmitter) {


  private var remaining: Int = 0

  private var timer: Int = 0

  eventEmitter.listen(CargoMade.name, _ => remaining = remaining + 1)

  def count:Int = {
    timer = 0
    eventEmitter.listen(Events.cargoDelivered, {
      case CargoDelivered(_, _, _: FinalDestination, _) => remaining = remaining - 1
      case _ =>
    })
    eventEmitter.emit(Start)
    while (!isCompleted){
      timer = timer + 1
      eventEmitter.emit(OneHourPassed(timer))
    }
    timer
  }

  def isCompleted: Boolean = remaining == 0

}

object TimeCounter{
  def apply(eventEmitter: EventEmitter): TimeCounter = new TimeCounter(eventEmitter)
}
