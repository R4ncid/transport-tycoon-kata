package eu.ddd.transportTycoon.infrastructure

import eu.ddd.transportTycoon.{CargoLoggerItem, FileLoggerItem}

object JsonSerializer {

  sealed trait JsonValue {
    def stringify: String
  }

  final case class JsonString(value: String) extends JsonValue {
    override def stringify: String = "\"" + value + "\""
  }

  final case class JsonNumber(value: Int) extends JsonValue {
    override def stringify: String = value.toString
  }

  final case class JsonArray(list: List[JsonValue]) extends JsonValue {
    override def stringify: String = list.map(_.stringify).mkString("[", ",", "]")
  }

  final case class JsonObject(obj: Map[String, JsonValue]) extends JsonValue {
    override def stringify: String = obj.map {
      case (key, value) => "\"" + key + "\":" + value.stringify
    }.mkString("{", ",", "}")
  }

  //2 implicit converters

  trait JsonConverter[T] {
    def convert(value: T): JsonValue
  }


  implicit class JsonOps[T](value: T) {
    def toJSON(implicit converter: JsonConverter[T]): JsonValue =
      converter.convert(value)
  }

  implicit object StringConverter extends JsonConverter[String] {
    override def convert(value: String): JsonValue = JsonString(value)
  }

  implicit object NumberConverter extends JsonConverter[Int] {
    override def convert(value: Int): JsonValue = JsonNumber(value)
  }

  implicit object CargoLoggerItemConverter extends JsonConverter[CargoLoggerItem] {
    override def convert(cargoLoggerItem: CargoLoggerItem): JsonValue = JsonArray(List(
      JsonObject(Map(
        "cargo_id" -> cargoLoggerItem.cargo_id.toJSON,
        "destination" -> cargoLoggerItem.destination.toJSON,
        "origin" -> cargoLoggerItem.origin.toJSON,
      ))))
  }

  implicit object FileLoggerItemConverter extends JsonConverter[FileLoggerItem] {
    override def convert(fileLoggerItem: FileLoggerItem): JsonValue = {
      val map = Map(
        "event" -> fileLoggerItem.event.toJSON,
        "time" -> fileLoggerItem.time.toJSON,
        "transport_id" -> fileLoggerItem.transport_id.toJSON,
        "kind" -> fileLoggerItem.kind.toJSON,
        "location" -> fileLoggerItem.location.toJSON,
      )
      val destinationMap = fileLoggerItem.destination match {
        case Some(value) => Map("destination" -> value.toJSON)
        case None => Map()
      }

      val cargoMap = fileLoggerItem.cargo match {
        case Nil => Map()
        case ::(cargoLoggerItem: CargoLoggerItem, _) => Map("cargo" -> cargoLoggerItem.toJSON)
      }

      JsonObject(cargoMap ++ destinationMap ++ map)
    }
  }

}
