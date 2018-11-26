package object model {
  type MaximumLevel = Map[String, String]

  case class TarifsData(ck1: Ck1,
                        ck1kp: Ck1,
                        ck2: Ck2,
                        ck2kp: Ck2,
                        ck3: Ck3,
                        ck3kp: Ck3,
                        ck4: Ck4,
                        ck4kp: Ck4,
                        ck5: Ck5,
                        ck5kp: Ck5,
                        ck6: Ck6,
                        ck6kp: Ck6)

  type RateE = Array[String]

  type RateP = String

  case class Ck3(rateE_BH: RateE, rateE_CHI: RateE, rateE_CHII: RateE, rateE_HH: RateE, rateP: RateP)

  case class Ck4(rateE_BH: RateE, rateE_CHI: RateE, rateE_CHII: RateE, rateE_HH: RateE, rateP: RateP, rateTransport: Map[String, String])

  case class Ck5(rateE_BH: RateE, rateE_CHI: RateE, rateE_CHII: RateE, rateE_HH: RateE, rateExcessFact: RateE, rateExcessPlan: RateE, ratePlan: String, rateAbs: String, rateP: RateP)

  case class Ck6(rateE_BH: RateE, rateE_CHI: RateE, rateE_CHII: RateE, rateE_HH: RateE, rateExcessFact: RateE, rateExcessPlan: RateE, ratePlan: String, rateAbs: String, rateP: RateP, rateTransport: Map[String, String])

  case class Ck1(maximumLevel: MaximumLevel)

  case class Ck2(maximumLevel3Zone: MaximumLevel3Zone, maximumLevel2Zone: MaximumLevel2Zone)

  case class MaximumLevel3Zone(night: MaximumLevel, peak: MaximumLevel, halfPeak: MaximumLevel)

  case class MaximumLevel2Zone(night: MaximumLevel, peak: MaximumLevel)
}
