package org.virtuslab.blog.kleisli

import java.util.Date
import java.util.logging.Logger

import Kleisli._
import Arrow._
import Monad._
import Choice._

import scala.language.higherKinds
import scala.util.{ Success, Failure, Try }

class ProductionLotsService(productionLotsRepository: ProductionLotsRepository) {
  private val logger = Logger.getLogger(this.getClass.getName)

  private def productionLotArrow[Env](verify: (ProductionLot, Env) => Either[Error, ProductionLot],
                                      copy: (ProductionLot, Env) => ProductionLot,
                                      log: (ProductionLot, Env) => Unit): Env => Long => Either[Error, Long] = {
    type Track[T] = Either[Error, T]
    def track[A, B](f: A => Track[B]) = Kleisli[Track, A, B](f)

    val getFromDb = track { productionLotsRepository.findExistingById }
    val validate = (env: Env) => track { verify(_: ProductionLot, env) } >>> track { verifyProductionLotNotDone }
    val save = Kleisli { productionLotsRepository.save } transform new (Try ~> Track) {
      override def apply[A](fa: Try[A]): Track[A] = fa match {
        case Success(a)  => Right(a)
        case Failure(ex) => Left(ProductionLotUpdateError(ex))
      }
    }

    val logError: Error => Unit = error => logger.warning(s"Cannot perform operation on production lot: $error")
    val logSuccess: Long => Unit = id => logger.fine(s"Production lot $id updated")

    (env: Env) =>
      ((
        getFromDb -| (log(_, env))
        >>> validate(env)).map(copy(_, env))
        >>> save)
        .run -| (logError ||| logSuccess)
  }

  private case class StartProduction(productionStartDate: Date, workerId: Long)
  private val startProductionA = productionLotArrow[StartProduction](
    (pl, env) => verifyWorkerCanBeAssignedToProductionLot(pl, env.workerId),
    (pl, env) => pl.copy(
      productionStartDate = Some(env.productionStartDate),
      workerId = Some(env.workerId),
      status = ProductionLotStatus.InProduction
    ),
    (pl, env) => logger.fine(s"Starting production of $pl on ${env.productionStartDate} by ${env.workerId}")
  )

  def startProductionOf(productionLotId: Long, productionStartDate: Date, workerId: Long): Either[Error, Long] =
    startProductionA(StartProduction(productionStartDate, workerId))(productionLotId)

  private val changeWorkerA = productionLotArrow[Long] (verifyWorkerChange,
    (pl, id) => pl.copy(workerId = Some(id)),
    (pl, id) => logger.fine(s"Changing worker of $pl to $id"))

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
      ),
      (pl, status) => logger.fine(s"Revoking $pl to $status")
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
