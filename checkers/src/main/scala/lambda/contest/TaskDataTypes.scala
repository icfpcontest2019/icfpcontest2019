package lambda.contest

import lambda.geometry.integer.{IPoint, IPolygon}

/**
  * @author Ilya Sergey
  */
object TaskDataTypes {

  object Booster extends Enumeration {
    type Booster = Value

    val BatteriesBooster, CoffeeBooster, DrillBooster,
    PortalBooster, CallFriendBooster, CallPoint = Value

  }

  case class ContestTask(room: IPolygon, obstacles: Seq[IPolygon], boosters: Seq[(Booster.Value, IPoint)])

}
