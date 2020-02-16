package eu.ddd.transportTycoon

import eu.ddd.transportTycoon.domain.{Arrived, Departed, Event, Factory, RouteWithDestination}

object FileLogItemTransformer {

  trait Transformer {
    def transform:Option[FileLoggerItem]
  }

  implicit class ArrivedFileLogTransformer(event: Arrived) extends Transformer {
    override def transform: Option[FileLoggerItem] = {
      val destination = event.route match {
        case RouteWithDestination(_, to, _) => Some(to.toString)
        case _ => None
      }
      Some(
        FileLoggerItem(
          "ARRIVE",
          event.time,
          event.vehicle.id,
          event.vehicle.kind,
          destination.get.toString,
          None,
          event.cargo match {
            case Some(cargo) => List(CargoLoggerItem(
              cargo.id,
              cargo.destination.toString,
              Factory.toString
            ))
            case None => List()
          }
        )
      )
    }
  }

  implicit class DepartedFileLogTransformer(event: Departed) extends Transformer {
    override def transform: Option[FileLoggerItem] = {
      val location = event.route.location.toString

      val destination = event.route match {
        case RouteWithDestination(from, to, time) => Some(from.toString)
        case _ => None
      }

      Some(FileLoggerItem(
        "DEPART",
        event.time,
        event.vehicle.id,
        event.vehicle.kind,
        location,
        destination,
        event.cargo match {
          case Some(cargo) => List(CargoLoggerItem(
            cargo.id,
            cargo.destination.toString,
            Factory.toString
          ))
          case None => List()
        }
      )
      )
    }
  }

  implicit class EventTransformer(e: Event) extends Transformer {
    override def transform: Option[FileLoggerItem] = e match {
      case a: Arrived => a.transform
      case d: Departed => d.transform
      case _ => None
    }
  }
}
