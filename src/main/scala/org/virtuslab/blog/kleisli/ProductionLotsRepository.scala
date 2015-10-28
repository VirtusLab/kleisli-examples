package org.virtuslab.blog.kleisli

import scala.util.Try

class ProductionLotsRepository {
  def findExistingById(productionLotId: Long): Either[Error, ProductionLot] =
    findById(productionLotId).toRight(ProductionLotNotFoundError(productionLotId))

  def findById(productionLotId: Long): Option[ProductionLot] = ???

  def save(productionLot: ProductionLot): Try[Long] = ???
}