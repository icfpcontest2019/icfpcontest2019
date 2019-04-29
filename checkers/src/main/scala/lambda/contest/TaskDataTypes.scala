package lambda.contest

import lambda.geometry.integer.{IPoint, IPolygon}

/**
  * @author Ilya Sergey
  */
object TaskDataTypes {

  abstract class Booster
  case object Batteries extends Booster
  case object Coffee extends Booster
  case object Drill extends Booster
  case object Portal extends Booster
  case object CallFriend extends Booster
  case object CallPoint extends Booster


  case class ContestTask(room: IPolygon, obstacles: Seq[IPolygon], boosters: Seq[(Booster, IPoint)])

}
