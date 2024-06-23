package com.tst.service

import org.specs2.mutable.Specification

class PromotionServiceSpec extends Specification {
  val promotionService = new PromotionService()

  "allCombinablePromotions" >> {
    "the unit test that was provided on the assignment" >> {
      val promotions = Seq(
        Promotion("P1", Seq("P3")),
        Promotion("P2", Seq("P4", "P5")),
        Promotion("P3", Seq("P1")),
        Promotion("P4", Seq("P2")),
        Promotion("P5", Seq("P2"))
      )


      val results = promotionService.allCombinablePromotions(promotions).map(codes => PromotionCombo(codes.promotionCodes.sorted))

      results.size must_== 4

      val expectedResult = Seq(
        PromotionCombo(Seq("P1", "P2")),
        PromotionCombo(Seq("P1", "P4", "P5")),
        PromotionCombo(Seq("P2", "P3")),
        PromotionCombo(Seq("P3", "P4", "P5"))
      )

      results must contain(exactly(expectedResult: _*))
    }

    "other unit tests" >> {
       "test for empty rates or/and prices" >> {
         val promotions = Seq.empty[Promotion]

         val results = promotionService.allCombinablePromotions(promotions)
         results.size must_== 0
       }

      "test that promotion that doesn't combine together not return as a single Sequence" >> {
        val promotions = Seq(
          Promotion("P1", Seq("P2")),
          Promotion("P2", Seq("P1")))

        val results = promotionService.allCombinablePromotions(promotions).map(codes => PromotionCombo(codes.promotionCodes.sorted))
        results.size must_== 0
      }
    }
  }

  "combinablePromotions" >> {
    "the unit test that was provided on the assignment" >> {
      val promotions = Seq(
        Promotion("P1", Seq("P3")),
        Promotion("P2", Seq("P4", "P5")),
        Promotion("P3", Seq("P1")),
        Promotion("P4", Seq("P2")),
        Promotion("P5", Seq("P2"))
      )


      val p1Results = promotionService.combinablePromotions("P1", promotions).map(codes => PromotionCombo(codes.promotionCodes.sorted))
      val p3Results = promotionService.combinablePromotions("P3", promotions).map(codes => PromotionCombo(codes.promotionCodes.sorted))

      p1Results.size must_== 2
      p3Results.size must_== 2

      val p1ExpectedResult = Seq(
        PromotionCombo(Seq("P1", "P2")),
        PromotionCombo(Seq("P1", "P4", "P5"))
      )

      val p3ExpectedResult = Seq(
        PromotionCombo(Seq("P2", "P3")),
        PromotionCombo(Seq("P3", "P4", "P5"))
      )

      p1Results must contain(exactly(p1ExpectedResult: _*))
      p3Results must contain(exactly(p3ExpectedResult: _*))
    }
  }

}
