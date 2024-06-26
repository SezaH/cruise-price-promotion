package com.tst.service

case class Promotion(code: String, notCombinableWith: Seq[String])
case class PromotionCombo(promotionCodes: Seq[String])

class PromotionService {
  def allCombinablePromotions(allPromotions: Seq[Promotion]): Seq[PromotionCombo] = {

    val codeToPromotion = allPromotions.map(promotion => promotion.code -> promotion).toMap

    def canCombine(promotionCodes: Seq[String], promotion: Promotion): Boolean =
      promotionCodes.forall { code =>
        !promotion.notCombinableWith.contains(code) &&
          !codeToPromotion(code).notCombinableWith.contains(promotion.code)
      }

    def findSubSequences(promotions: Seq[Promotion], currentPromotionCodes: Seq[String]): Seq[PromotionCombo] = {
      if (promotions.isEmpty) {
        Seq(PromotionCombo(currentPromotionCodes))
      } else {
        val promotion = promotions.head
        val restPromotions = promotions.tail

        val combosWithCurrent = if (canCombine(currentPromotionCodes, promotion)) {
          findSubSequences(restPromotions, currentPromotionCodes :+ promotion.code)
        } else {
          Seq.empty
        }
        val combosWithoutCurrent = findSubSequences(restPromotions, currentPromotionCodes)

        // Combine results from both branches
        combosWithCurrent ++ combosWithoutCurrent
      }
    }

    val allCombinableSubSeq = findSubSequences(allPromotions, Seq())

    val filteredCombos = allCombinableSubSeq.filter(_.promotionCodes.length > 1) // The assumption is the code that doesn't combine with other code shouldn't return at all.

    filteredCombos.foldLeft(Seq.empty[PromotionCombo]) { (acc, comboCodes) =>
      if (acc.exists(combo => comboCodes.promotionCodes.forall(combo.promotionCodes.contains))) acc
      else acc :+ comboCodes
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
