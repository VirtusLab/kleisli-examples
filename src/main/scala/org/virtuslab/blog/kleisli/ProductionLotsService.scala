package org.virtuslab.blog.kleisli

import java.util.Date

import org.virtuslab.blog.kleisli.Arrow._

class ProductionLotsService(productionLotsRepository: ProductionLotsRepository) {

  private def productionLotArrow[Env](verify: (ProductionLot, Env) => Unit,
                                      copy: (ProductionLot, Env) => ProductionLot): (Long, Env) => Long = {
    val verifyProductionLotNotDoneF: ((ProductionLot, Env)) => Unit = { case (pl, _) => verifyProductionLotNotDone(pl) }

    Function.untupled(
      (productionLotsRepository.findExistingById _ *** identity[Env])
        >>> ((verify.tupled &&& verifyProductionLotNotDoneF)
          &&& (copy.tupled >>> productionLotsRepository.save))
          >>> (_._2)
    )
  }

  private case class StartProduction(productionStartDate: Date, workerId: Long)
  private val startProductionA = productionLotArrow[StartProduction] (
    (pl, env) => verifyWorkerCanBeAssignedToProductionLot(pl, env.workerId),
    (pl, env) => pl.copy(
      productionStartDate = Some(env.productionStartDate),
      workerId = Some(env.workerId),
      status = ProductionLotStatus.InProduction
    )
  )

  def startProductionOf(productionLotId: Long, productionStartDate: Date, workerId: Long): Unit =
    startProductionA(productionLotId, StartProduction(productionStartDate, workerId))

  private val changeWorkerA = productionLotArrow[Long] (verifyWorkerChange, (pl, id) => pl.copy(workerId = Some(id)))

  def changeAssignedWorker(productionLotId: Long, newWorkerId: Long): Unit =
    changeWorkerA(productionLotId, newWorkerId)

  private val revokeToA =
    productionLotArrow[ProductionLotStatus.Value] (
      (_, _) => (),
      (pl, status) => pl.copy(
        status = status,
        workerId = None,
        productionStartDate = None,
        productionEndDate = None
      )
    )

  def revokeProductionLotTo(productionLotId: Long,
                            productionLotStatus: ProductionLotStatus.Value): Unit =
    revokeToA(productionLotId, productionLotStatus)

  private def verifyProductionLotNotDone(productionLot: ProductionLot): Either[Error, ProductionLot] =
    Either.cond(productionLot.status != ProductionLotStatus.Done, productionLot, ProductionLotClosedError(productionLot))

  private def verifyWorkerChange(productionLot: ProductionLot, newWorkerId: Long): Either[Error, ProductionLot] =
    productionLot.workerId.fold[Either[Error, ProductionLot]](
      Left(NoWorkerError(productionLot)))(
        workerId => Either.cond(workerId != newWorkerId, productionLot, SameWorkerError(productionLot)))

  private def verifyWorkerCanBeAssignedToProductionLot(productionLot: ProductionLot, workerId: Long): Either[Error, ProductionLot] =
    Either.cond(productionLot.workerId.isEmpty, productionLot, WorkerAlreadyAssignedError(productionLot))
}
