package reader

import java.io.File

import org.apache.poi.ss.usermodel.WorkbookFactory

object XlsxToCsv {

  case class CsvWorkbook(sheets: List[Iterable[String]])

  def convert(fileName: String): Iterable[Iterable[String]] = {
    // Automatically convert Java collections to Scala equivalents
    import scala.collection.JavaConversions._
    val workbook = WorkbookFactory.create(new File(fileName))
    val csv = workbook.map(sheet => sheet.map(row => {
      row.map(cell => cell).mkString(";")
    }))
    csv
  }

}
