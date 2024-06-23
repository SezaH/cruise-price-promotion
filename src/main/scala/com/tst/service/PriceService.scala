package com.tst.service

// Potentially these case classes should have their own file inside model package but for sake of simplicity
// for this problem we keep it here.
case class Rate(rateCode: String, rateGroup: String)

case class CabinPrice(
  cabinCode: String,
  rateCode: String,
  price: BigDecimal)

case class BestGroupPrice(
  cabinCode: String,
  rateCode: String,
  price: BigDecimal,
  rateGroup: String)

class PriceService {

  def getBestGroupPrices(
    rates: Seq[Rate],
    prices: Seq[CabinPrice]
  ): Seq[BestGroupPrice] = {
    val rateCodeToPrices = prices.groupBy(_.rateCode)
    rates.flatMap { rate =>
      rateCodeToPrices.getOrElse(rate.rateCode, Seq()).map { cabinPrice =>
        BestGroupPrice(
          cabinCode = cabinPrice.cabinCode,
          rateCode = rate.rateCode,
          price = cabinPrice.price,
          rateGroup = rate.rateGroup)
      }
    }.groupBy(bgp => (bgp.cabinCode, bgp.rateGroup))
      .view
      .mapValues(_.minBy(_.price))
      .values
      .toSeq
  }

}
