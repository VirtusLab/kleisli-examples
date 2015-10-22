package org.virtuslab.blog.kleisli

import java.util.Date

import Kleisli._
import Arrow._
import Monad._

class ProductionLotsService(productionLotsRepository: ProductionLotsRepository) {
  type E[R] = Either[Error, R]

  private def productionLotArrow[Env](verify: (ProductionLot, Env) => E[ProductionLot],
                                      copy: (ProductionLot, Env) => ProductionLot): Env => Long => E[Long] = {
    val verifyProductionLotNotDoneF: (ProductionLot) => E[ProductionLot] = verifyProductionLotNotDone

    (env: Env) => (
      Kleisli[E, Long, ProductionLot]{ productionLotsRepository.findExistingById }
      >>> Kleisli { verify(_, env) }
      >>> Kleisli { verifyProductionLotNotDoneF }
    ).map(copy(_, env)) >==> productionLotsRepository.save _
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

  def startProductionOf(productionLotId: Long, productionStartDate: Date, workerId: Long): Either[Error, Long] =
    startProductionA(StartProduction(productionStartDate, workerId))(productionLotId)

  private val changeWorkerA = productionLotArrow[Long] (verifyWorkerChange, (pl, id) => pl.copy(workerId = Some(id)))

  def changeAssignedWorker(productionLotId: Long, newWorkerId: Long): Either[Error, Long] =
    changeWorkerA(newWorkerId)(productionLotId)

  private val revokeToA =
    productionLotArrow[ProductionLotStatus.Value] (
      (pl, _) => Right(pl),
      (pl, status) => pl.copy(
        status = status,
        workerId = None,
        productionStartDate = None,
        productionEndDate = None
      )
    )

  def revokeProductionLotTo(productionLotId: Long,
                            productionLotStatus: ProductionLotStatus.Value): Either[Error, Long] =
    revokeToA(productionLotStatus)(productionLotId)

  private def verifyProductionLotNotDone(productionLot: ProductionLot): Either[Error, ProductionLot] =
    Either.cond(productionLot.status != ProductionLotStatus.Done, productionLot, ProductionLotClosedError(productionLot))

  private def verifyWorkerChange(productionLot: ProductionLot, newWorkerId: Long): Either[Error, ProductionLot] =
    productionLot.workerId.fold[Either[Error, ProductionLot]](
      Left(NoWorkerError(productionLot)))(
        workerId => Either.cond(workerId != newWorkerId, productionLot, SameWorkerError(productionLot)))

  private def verifyWorkerCanBeAssignedToProductionLot(productionLot: ProductionLot, workerId: Long): Either[Error, ProductionLot] =
    Either.cond(productionLot.workerId.isEmpty, productionLot, WorkerAlreadyAssignedError(productionLot))
}
