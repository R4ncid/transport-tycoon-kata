package eu.ddd.transportTycoon

import eu.ddd.transportTycoon.domain.{Event, EventName}


case class EventEmitter() {

  type Listener = Event => Unit

  var listeners: Map[EventName, List[Listener]] = Map()

  def listen(eventName: EventName, listener: Listener): Unit = {
    val actualListener = listeners.getOrElse(eventName, List())
    listeners = listeners + (eventName -> (actualListener ::: List(listener)))
  }

  def emit(event: Event): Unit =
  listeners.getOrElse(event.name, List()).foreach(l => l(event))
}
