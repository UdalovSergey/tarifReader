import model._

//TODO: get by index, call head after drop and slice are unsafe.
//Run time exception here means, that file has wrong format.
//Add error handling
package object Parser {

  val currentLevels = List("BH", "CHI", "CHII", "HH")

  def parseRateE(idx: Int, rawValues: List[Iterable[Array[String]]]): RateE = {
    val values = rawValues(idx).flatMap(day => day.drop(1).map(s => s.replace(" ", "")))
    values.toArray
  }

  def parseRateP(idx: Int, rawValues: List[Iterable[Array[String]]]): RateP = {
    val value = rawValues(idx).last.last.replace(" ", "")
    value
  }

  def parseMaximumLevelUnregPrices(rawValues: List[Iterable[Array[String]]]): List[(String, String)] = {
    currentLevels.zip(
      rawValues.head.slice(7, 8).toList.head
        .slice(3, 7)
    )
  }

  def parseMaximumLevelUnregPrices3Zones(idx: Int, rawValues: List[Iterable[Array[String]]]): MaximumLevel3Zone = {
    val night = currentLevels.zip(
      rawValues(idx).head.toList
        .slice(1, 5)
    ).toMap

    val peak = currentLevels.zip(
      rawValues(idx).drop(1).head.toList
        .slice(1, 5)
    ).toMap

    val halfPeak = currentLevels.zip(
      rawValues(idx).drop(2).head.toList
        .slice(1, 5)
    ).toMap
    MaximumLevel3Zone(night, peak, halfPeak)
  }

  def parseMaximumLevelUnregPrices2Zones(idx: Int, rawValues: List[Iterable[Array[String]]]): MaximumLevel2Zone = {
    val night = currentLevels.zip(
      rawValues(idx).head.toList
        .slice(1, 5)
    ).toMap

    val peak = currentLevels.zip(
      rawValues(idx).drop(1).head.toList
        .slice(1, 5)
    ).toMap

    MaximumLevel2Zone(night, peak)
  }

  def parseRateTransport(idx: Int, rawValues: List[Iterable[Array[String]]]): Map[String, String] = {
    currentLevels.zip(
      rawValues(idx).last.drop(1).filterNot(s => s.length == 0)
    ).toMap
  }

  def parseRatePlan(rawValues: Array[String]): String = {
    rawValues.last
  }

  def parseRateAbs(rawValues: Array[String]): String = {
    rawValues.last
  }

}
