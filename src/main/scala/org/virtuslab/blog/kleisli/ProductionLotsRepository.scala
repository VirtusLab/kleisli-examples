package org.virtuslab.blog.kleisli

class ProductionLotsRepository {
  def findExistingById(productionLotId: Long): ProductionLot =
    findById(productionLotId).getOrElse(sys.error(s"ProductionLot $productionLotId not found"))

  def findById(productionLotId: Long): Option[ProductionLot] = ???

  def save(productionLot: ProductionLot): Long = ???
}