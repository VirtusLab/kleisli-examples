package org.virtuslab.blog.kleisli

import java.util.Date

class ProductionLotsService(productionLotsRepository: ProductionLotsRepository) {
  def startProductionOf(productionLotId: Long, productionStartDate: Date, workerId: Long): Unit = {
    val productionLot = productionLotsRepository.findExistingById(productionLotId)

    verifyWorkerCanBeAssignedToProductionLot(productionLot, workerId)

    val productionLotWithProductionStartData = productionLot.copy(
      productionStartDate = Some(productionStartDate),
      workerId = Some(workerId),
      status = ProductionLotStatus.InProduction
    )

    productionLotsRepository.save(productionLotWithProductionStartData)
  }

  def changeAssignedWorker(productionLotId: Long, newWorkerId: Long): Unit = {
    val productionLot = productionLotsRepository.findExistingById(productionLotId)

    verifyWorkerChange(productionLot, newWorkerId)

    val updatedProductionLot = productionLot.copy(workerId = Some(newWorkerId))

    productionLotsRepository.save(updatedProductionLot)
  }

  def revokeProductionLotTo(productionLotId: Long,
                            productionLotStatus: ProductionLotStatus.Value): Unit = {
    val productionLot = productionLotsRepository.findExistingById(productionLotId)

    val revokedProductionLot = productionLot.copy(
      status = productionLotStatus,
      workerId = None,
      productionStartDate = None,
      productionEndDate = None
    )

    productionLotsRepository.save(revokedProductionLot)
  }

  private def verifyWorkerChange(productionLot: ProductionLot, newWorkerId: Long): Unit = {
    require(productionLot.workerId.isDefined && productionLot.workerId.get != newWorkerId,
            s"Production lot worker expected to be defined and different than $newWorkerId")
  }

  private def verifyWorkerCanBeAssignedToProductionLot(productionLot: ProductionLot, workerId: Long): Unit = {
    val productionLotId = productionLot.id.get
    val productionLotHasNoWorkerAssigned = productionLot.workerId.isEmpty

    require(productionLotHasNoWorkerAssigned, s"Production lot: $productionLotId has worker already assigned")
  }
}
