
package reader

import model._
import scala.util.{Failure, Success, Try}

//TODO: get by index, call head after drop and slice are unsafe.
//Run time exception here means, that file has wrong format.
//Add error handling
object Reader {

  val ck1SheetIdx = 0
  val ck1kpSheetIdx = 1
  val ck2SheetIdx = 2
  val ck2kpSheetIdx = 3
  val ck3SheetIdx = 4
  val ck3kpSheetIdx = 5
  val ck4SheetIdx = 6
  val ck4kpSheetIdx = 7
  val ck5SheetIdx = 8
  val ck5kpSheetIdx = 9
  val ck6SheetIdx = 10
  val ck6kpSheetIdx = 11


  def read(fileName: String): Try[TarifsData] = {
    try {
      val csvLines = XlsxToCsv.convert(fileName)
      val ck1 = createCk1(readCk1(csvLines, ck1SheetIdx))
      val ck1kp = createCk1(readCk1(csvLines, ck1kpSheetIdx))
      val ck2 = createCk2(readCk2(csvLines, ck2SheetIdx))
      val ck2kp = createCk2(readCk2(csvLines, ck2kpSheetIdx))
      val ck3 = createCk3(readCk3(csvLines, ck3SheetIdx))
      val ck3kp = createCk3(readCk3(csvLines, ck3kpSheetIdx))
      val ck4 = createCk4(readCk3(csvLines, ck4SheetIdx))
      val ck4kp = createCk4(readCk3(csvLines, ck4kpSheetIdx))
      val ck5 = createCk5(readCk3(csvLines, ck5SheetIdx))
      val ck5kp = createCk5(readCk3(csvLines, ck5kpSheetIdx))
      val ck6 = createCk6(readCk3(csvLines, ck6SheetIdx))
      val ck6kp = createCk6(readCk3(csvLines, ck6kpSheetIdx))
      Success(TarifsData(ck1, ck1kp, ck2, ck2kp, ck3, ck3kp, ck4, ck4kp, ck5, ck5kp, ck6, ck6kp))
    } catch {
      case e: Exception => Failure(e)
    }
  }


  def readCk1(csvSheets: Iterable[Iterable[String]], sheetIdx: Int): List[Iterable[Array[String]]] = {
    val ck1Sheet = csvSheets.toList(sheetIdx)
    val lines = splitLines(ck1Sheet)
    List(lines)
  }

  def readCk2(csvSheets: Iterable[Iterable[String]], sheetIdx: Int): List[Iterable[Array[String]]] = {
    val ck2Sheet = csvSheets.toList(sheetIdx)
    val lines = splitLines(ck2Sheet)
    val linesFor3Zones = lines.slice(6, 9)
    val linesFor2Zones = lines.slice(14, 16)
    List(linesFor3Zones, linesFor2Zones)
  }

  def readCk3(csvSheets: Iterable[Iterable[String]], sheetIdx: Int): List[Iterable[Array[String]]] = {
    val ck3Sheet = csvSheets.toList(sheetIdx)
    val lines = splitLines(ck3Sheet)
      .filter(l => !(l.length < 14 && l.length != 0))
      .filter(l => !(l.length > 1 && l(1) == "0:00-1:00")).toList //remove header for table

    val splitted = splitList(lines)(l => l.length != 0).filter(col => col != Nil && (col.forall(c => c.nonEmpty)))
    splitted
  }

  private def splitLines(sheet: Iterable[String]): Iterable[Array[String]] = {
    sheet.map(l => l.split(";").map(_.trim))
  }

  def splitList[T](x: Iterable[T])(pred: T => Boolean): List[Iterable[T]] = {
    x.span(pred) match {
      case (Nil, doesNotMatch :: unTested) =>
        List(doesNotMatch) :: splitList(unTested)(pred)
      case (matchingPrefix, doesNotMatch :: unTested) =>
        matchingPrefix :: List(doesNotMatch) :: splitList(unTested)(pred)
      case (matchingPrefix, Nil) => List(matchingPrefix)
    }
  }

  def createCk1(splitted: List[Iterable[Array[String]]]): Ck1 = {
    Ck1(Parser.parseMaximumLevelUnregPrices(splitted).toMap)
  }

  def createCk2(splitted: List[Iterable[Array[String]]]): Ck2 = {
    val value3Zone = Parser.parseMaximumLevelUnregPrices3Zones(0, splitted)
    val value2Zone = Parser.parseMaximumLevelUnregPrices2Zones(1, splitted)
    Ck2(value3Zone, value2Zone)
  }

  def createCk3(splitted: List[Iterable[Array[String]]]): Ck3 = {
    val rateVN = Parser.parseRateE(0, splitted)
    val rateCHI = Parser.parseRateE(1, splitted)
    val rateCHII = Parser.parseRateE(2, splitted)
    val rateHH = Parser.parseRateE(3, splitted)
    val rateP = Parser.parseRateP(4, splitted)

    Ck3(rateVN, rateCHI, rateCHII, rateHH, rateP)
  }

  def createCk4(splitted: List[Iterable[Array[String]]]): Ck4 = {
    val rateVN = Parser.parseRateE(0, splitted)
    val rateCHI = Parser.parseRateE(1, splitted)
    val rateCHII = Parser.parseRateE(2, splitted)
    val rateHH = Parser.parseRateE(3, splitted)
    val rateP = Parser.parseRateP(4, splitted)
    val rateTransport = Parser.parseRateTransport(5, splitted)

    Ck4(rateVN, rateCHI, rateCHII, rateHH, rateP, rateTransport)
  }

  def createCk5(splitted: List[Iterable[Array[String]]]): Ck5 = {
    val rateVN = Parser.parseRateE(0, splitted)
    val rateCHI = Parser.parseRateE(1, splitted)
    val rateCHII = Parser.parseRateE(2, splitted)
    val rateHH = Parser.parseRateE(3, splitted)
    val rateExcessFact = Parser.parseRateE(4, splitted)
    val rateExcessPlan = Parser.parseRateE(5, splitted)

    val ratePlan = Parser.parseRatePlan(splitted(6).slice(1, 2).head)
    val rateAbs = Parser.parseRateAbs(splitted(6).slice(2, 3).head)
    val rateP = Parser.parseRateP(7, splitted)

    Ck5(rateVN, rateCHI, rateCHII, rateHH, rateExcessFact, rateExcessPlan, ratePlan, rateAbs, rateP)
  }

  def createCk6(splitted: List[Iterable[Array[String]]]): Ck6 = {
    val rateVN = Parser.parseRateE(0, splitted)
    val rateCHI = Parser.parseRateE(1, splitted)
    val rateCHII = Parser.parseRateE(2, splitted)
    val rateHH = Parser.parseRateE(3, splitted)
    val rateExcessFact = Parser.parseRateE(4, splitted)
    val rateExcessPlan = Parser.parseRateE(5, splitted)

    val ratePlan = Parser.parseRatePlan(splitted(6).slice(1, 2).head)
    val rateAbs = Parser.parseRateAbs(splitted(6).slice(2, 3).head)
    val rateP = Parser.parseRateP(7, splitted)
    val rateTransport = Parser.parseRateTransport(8, splitted)

    Ck6(rateVN, rateCHI, rateCHII, rateHH, rateExcessFact, rateExcessPlan, ratePlan, rateAbs, rateP, rateTransport)
  }

}
