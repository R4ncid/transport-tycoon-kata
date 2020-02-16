package eu.ddd.transportTycoon.infrastructure

import java.io.FileWriter

import eu.ddd.transportTycoon.{FileLoggerItem, LogWriter}

case class FileLogWriter(filename: String) extends LogWriter {

  import JsonSerializer._

  val filePath = s"src/resources/$filename.json"

  val fw = new FileWriter(filePath)
  fw.write(s"# Deliver $filename\n")
  fw.close()


  override def write(fileLoggerItem: FileLoggerItem): Unit = {
    val fw = new FileWriter(filePath, true)
    fw.write(fileLoggerItem.toJSON.stringify + '\n')
    fw.close()
  }
}
