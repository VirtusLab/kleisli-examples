package org.virtuslab.blog.kleisli

import java.util.Date

import Kleisli._
import Arrow._
import Monad._

class ProductionLotsService(productionLotsRepository: ProductionLotsRepository) {

  private def productionLotArrow[Env](verify: (ProductionLot, Env) => Either[Error, ProductionLot],
                                      copy: (ProductionLot, Env) => ProductionLot): Env => Long => Either[Error, Long] = {
    type Track[T] = Either[Error, T]
    def track[A, B](f: A => Track[B]) = Kleisli[Track, A, B](f)

    val getFromDb = track { productionLotsRepository.findExistingById }
    val validate = (env: Env) => track { verify(_: ProductionLot, env) } >>> track { verifyProductionLotNotDone }
    val save = track { productionLotsRepository.save }

    (env: Env) => (
      getFromDb
      >>> validate(env)
    ).map(copy(_, env)) >>> save
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
