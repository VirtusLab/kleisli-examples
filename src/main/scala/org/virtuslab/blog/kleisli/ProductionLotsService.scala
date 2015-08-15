package org.virtuslab.blog.kleisli

import java.util.Date

class ProductionLotsService(productionLotsRepository: ProductionLotsRepository) {
  def startProductionOf(productionLotId: Long, productionStartDate: Date, workerId: Long): Unit = {
    val verify: ProductionLot => ProductionLot = { productionLot =>
      verifyWorkerCanBeAssignedToProductionLot(productionLot, workerId)
      productionLot
    }

    val copy: ProductionLot => ProductionLot = _.copy(
      productionStartDate = Some(productionStartDate),
      workerId = Some(workerId),
      status = ProductionLotStatus.InProduction
    )

    val startProductionOfF = productionLotsRepository.findExistingById _ andThen
      verify andThen
      copy andThen
      productionLotsRepository.save

    startProductionOfF(productionLotId)
  }

  def changeAssignedWorker(productionLotId: Long, newWorkerId: Long): Unit = {
    val verify: ProductionLot => ProductionLot = { productionLot =>
      verifyWorkerChange(productionLot, newWorkerId)
      productionLot
    }

    val copy: ProductionLot => ProductionLot = _.copy(workerId = Some(newWorkerId))

    val changedAssignedWorkerF = productionLotsRepository.findExistingById _ andThen
                              verify andThen
                              copy andThen
                              productionLotsRepository.save

    changedAssignedWorkerF(productionLotId)
  }

  def revokeProductionLotTo(productionLotId: Long,
                            productionLotStatus: ProductionLotStatus.Value): Unit = {
    val copy: ProductionLot => ProductionLot = _.copy(
      status = productionLotStatus,
      workerId = None,
      productionStartDate = None,
      productionEndDate = None
    )

    val revokeProductionLotToF = productionLotsRepository.findExistingById _ andThen
                                 copy andThen
                                 productionLotsRepository.save

    revokeProductionLotToF(productionLotId)
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
