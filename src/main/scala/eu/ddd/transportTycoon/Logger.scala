package eu.ddd.transportTycoon

import eu.ddd.transportTycoon.domain.{Event, EventName}
import eu.ddd.transportTycoon.infrastructure.JsonSerializer.{FileLoggerItemConverter, JsonOps}
sealed trait Logger {
  def log(event: Event): Unit
}

case class ConsoleLogger(eventEmitter: EventEmitter, events: List[EventName]) extends Logger {
  events.foreach(e => eventEmitter.listen(e, log))

  override def log(event: Event): Unit = {
    event match {
      case event: Event => println(event)
      case _ =>
    }
  }
}

trait LogWriter {
  def write(fileLoggerItem: FileLoggerItem): Unit
}

case object ConsoleLogWriter extends LogWriter {
  override def write(fileLoggerItem: FileLoggerItem): Unit = println(fileLoggerItem.toJSON.stringify)
}


case class FileLogger(writer: LogWriter, eventEmitter: EventEmitter, events: List[EventName]) extends Logger {
  import FileLogItemTransformer._
  events.foreach(ev => eventEmitter.listen(ev, log))

  override def log(event: Event): Unit = {
    val fileLoggerItemOption = event.transform
    fileLoggerItemOption.foreach(writer.write)
  }
}

