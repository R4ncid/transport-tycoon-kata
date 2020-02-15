package eu.ddd.transportTycoon

import eu.ddd.transportTycoon.domain.FinalDestination

class Timer(eventEmitter: EventEmitter) {

  private var remaining: Int = 0

  private var timer: Int = 0

  def start(toDeliver: Int):Int = {
    timer = 0
    remaining = toDeliver
    eventEmitter.listen(Events.cargoDelivered, {
      case CargoDelivered(_, _, destination: FinalDestination) => remaining = remaining - 1
      case _ =>
    })
    eventEmitter.fire(Start)
    while (!isCompleted){
      timer = timer + 1
      eventEmitter.fire(OneHourPassed(timer))
    }
    timer
  }

  def isCompleted: Boolean = remaining == 0

}

object Timer{
  def apply(eventEmitter: EventEmitter): Timer = new Timer(eventEmitter)
}
