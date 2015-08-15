package org.virtuslab.blog.kleisli

import java.util.Date

case class ProductionLot(id: Option[Long],
                         status: ProductionLotStatus.Value,
                         productionStartDate: Option[Date] = None,
                         productionEndDate: Option[Date] = None,
                         workerId: Option[Long] = None)

object ProductionLotStatus extends Enumeration {
  // ...
  val InProduction = Value
  val Done = Value
  //...
}