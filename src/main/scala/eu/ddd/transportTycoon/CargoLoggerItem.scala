package eu.ddd.transportTycoon

case class FileLoggerItem(
  event: String,
  time: Int,
  transport_id: Int,
  kind: String,
  location: String,
  destination: Option[String],
  cargo: List[CargoLoggerItem])


case class CargoLoggerItem(
cargo_id: Int,
destination: String,
origin: String)

