package com.tst.service

case class Promotion(code: String, notCombinableWith: Seq[String])
case class PromotionCombo(promotionCodes: Seq[String])

class PromotionService {
  /** Get a Sequence of T and returns all subsequences.
   * Note: Potentially this function can move to different utility class which will be used by whole application
   * */
  private def allSubsequences[T](seq: Seq[T]): Seq[Seq[T]] = {
    @scala.annotation.tailrec
    def helper(remaining: Seq[T], acc: Seq[Seq[T]]): Seq[Seq[T]] = {
      if (remaining.isEmpty) acc
      else {
        val head = remaining.head
        val newAcc = acc ++ acc.map(subseq => head +: subseq)
        helper(remaining.tail, newAcc)
      }
    }

    helper(seq, Seq(Seq.empty))
  }

  def allCombinablePromotions(allPromotions: Seq[Promotion]): Seq[PromotionCombo] = {

    def canCombine(promotions: Seq[Promotion]): Boolean = {
      promotions.forall { promotion =>
        promotions.forall { anotherPromotion =>
          promotions == anotherPromotion || !promotion.notCombinableWith.contains(anotherPromotion.code)
        }
      }
    }

    val allSubPromotions= allSubsequences(allPromotions)

    val filteredSubPromotions = allSubPromotions
      .filter(canCombine)
      .filter(_.size > 1) // the assumption here is a promotion that doesn't combine with other promotions shouldn't return at all.

    filteredSubPromotions
      .filter { subPromotion =>
        !filteredSubPromotions.exists(anotherPromotion => subPromotion.size < anotherPromotion.size && subPromotion.forall(anotherPromotion.contains))
      }
      .map { promotions =>
        PromotionCombo(promotions.map(_.code))
      }
  }


  def combinablePromotions(
    promotionCode: String,
    allPromotions: Seq[Promotion]
  ): Seq[PromotionCombo] = {
    val allCombination = allCombinablePromotions(allPromotions)

    allCombination.filter(_.promotionCodes.contains(promotionCode))
  }
}
