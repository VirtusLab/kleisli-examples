package org.virtuslab.blog.kleisli

sealed abstract class Error(val message: String)

case class ProductionLotNotFoundError(id: Long) extends Error(s"ProductionLot $id does not exist")
case class ProductionLotClosedError(pl: ProductionLot) extends Error(s"Attempt to operate on finished ProductionLot $pl")
case class NoWorkerError(pl: ProductionLot) extends Error(s"No worker has been assigned to $pl")
case class SameWorkerError(pl: ProductionLot) extends Error(s"Illegal worker reassignment $pl")
case class WorkerAlreadyAssignedError(pl: ProductionLot) extends Error(s"Worker already assigned: $pl")
case class ProductionLotUpdateError(ex: Throwable) extends Error(s"Cannot update production lot: $ex")
