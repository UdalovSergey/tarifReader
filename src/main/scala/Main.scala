import java.io.{File, PrintWriter}

import reader.{Reader}


object Main extends App {
  val inputFileName = args(0)
  val outputFileName = args(1)

  val timing = new StringBuffer
  val job: Unit = timed("XLS to csv to json", doReading())
  println(timing)

  def doReading(): Unit = {

    Reader.read(inputFileName) match {
      case scala.util.Success(value) => saveJson(outputFileName, Serializer.gson.toJson(value))
      case scala.util.Failure(exception) => saveJson(outputFileName,
        Serializer.gson.toJson(Map("error" -> exception.toString)))
    }

  }

  def saveJson(fileName: String, value: String) = {
    val pw = new PrintWriter(new File(fileName))
    pw.write(value)
    pw.close
  }

  def timed[T](label: String, code: => T): T = {
    val start = System.currentTimeMillis()
    val result = code
    val stop = System.currentTimeMillis()
    timing.append(s"Processing $label took ${stop - start} ms.\n")
    result
  }
}
