package com.tst.service

import org.specs2.mutable.Specification

class PriceServiceSpec extends Specification {
  val priceService = new PriceService()

  "getBestGroupPrices" >> {
    "the unit test that was provided on the assignment" >> {
      val rates = Seq(
        Rate("M1", "Military"),
        Rate("M2", "Military"),
        Rate("S1", "Senior"),
        Rate("S2", "Senior")
      )

      val cabinPrices = Seq(
        CabinPrice("CA", "M1", 200.00),
        CabinPrice("CA", "M2", 250.00),
        CabinPrice("CA", "S1", 225.00),
        CabinPrice("CA", "S2", 260.00),
        CabinPrice("CB", "M1", 230.00),
        CabinPrice("CB", "M2", 260.00),
        CabinPrice("CB", "S1", 245.00),
        CabinPrice("CB", "S2", 270.00)
      )

      val results = priceService.getBestGroupPrices(rates, cabinPrices)

      results.size must_== 4

      val expectedResult = Seq(
        BestGroupPrice("CA", "M1", 200.00, "Military"),
        BestGroupPrice("CA", "S1", 225.00, "Senior"),
        BestGroupPrice("CB", "M1", 230.00, "Military"),
        BestGroupPrice("CB", "S1", 245.00, "Senior")
      )

      results must contain(exactly(expectedResult: _*))
    }

    "other unit tests" >> {
       "test for empty rates or/and prices" >> {
         val rates = Seq.empty[Rate]
         val prices = Seq.empty[CabinPrice]

         val nonEmptyRates = Seq(
           Rate("M1", "Military"),
           Rate("M2", "Military"),
           Rate("S1", "Senior"),
           Rate("S2", "Senior")
         )

         val nonEmptyPrices = Seq(
           CabinPrice("CA", "M1", 200.00),
           CabinPrice("CA", "M2", 250.00),
           CabinPrice("CA", "S1", 225.00),
           CabinPrice("CA", "S2", 260.00),
           CabinPrice("CB", "M1", 230.00),
           CabinPrice("CB", "M2", 260.00),
           CabinPrice("CB", "S1", 245.00),
           CabinPrice("CB", "S2", 270.00)
         )

         val results =  priceService.getBestGroupPrices(rates, prices)
         results.size must_== 0

         val results1 =  priceService.getBestGroupPrices(nonEmptyRates, prices)
         results1.size must_== 0

         val results2 =  priceService.getBestGroupPrices(rates, nonEmptyPrices)
         results2.size must_== 0
       }

      "test for no matching prices for rates" >> {
        val rates = Seq(Rate("M1", "Military"))
        val prices = Seq(CabinPrice("CA", "S2", 260.00))

        val results =  priceService.getBestGroupPrices(rates, prices)
        results.size must_== 0
      }

      "test for getting the best price" >> {
        val rates = Seq(Rate("M1", "Military"))
        val prices = Seq(
          CabinPrice("CA", "M1", 260.00),
          CabinPrice("CA", "M1", 210.00),
          CabinPrice("CA", "M1", 205.00))

        val results = priceService.getBestGroupPrices(rates, prices)

        results.size must_== 1

        val expectedResult = Seq(
          BestGroupPrice("CA", "M1", 205.00, "Military")
        )

        results must contain(exactly(expectedResult: _*))
      }
    }
  }

}
