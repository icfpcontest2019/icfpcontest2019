package lambda.contest.checkers

import lambda.contest.ErrorMessages._
import lambda.contest.{Booster, ContestException, ContestTask}
import lambda.geometry.integer.{IPoint, IPolygon}

/**
  * @author Ilya Sergey
  */
object ContestCheckingUtils {

  /**
    * Check if the task is well-formed
    */
  def checkTaskWellFormed(task: ContestTask): Boolean = {

    val ContestTask(room, initPos, obstacles, boosterPositions) = task

    // Check room
    if (!room.isWellFormed) throw ContestException(MALFORMED_ROOM)
    if (!room.isRectilinear) throw ContestException(NON_RECTILINEAR_ROOM)
    if (!room.containsCell(initPos)) throw ContestException(BAD_INIT_POS)
    if (!roomWithinPositiveBoundingBox(room)) throw ContestException(BOUNDING_BOX)

    // Check obstacles
    for (i <- obstacles.indices; o = obstacles(i)) {

      if (!checkObstacle(o))
        throw ContestException(MALFORMED_OBSTACLE)

      if (o.containsCell(initPos))
        throw ContestException(INIT_POS_OBSTACLE)

      if (!room.containsPolygonProperly(o))
        throw ContestException(OBSTACLE_NOW_WITHIN)

      // Obstacles do not intersect or contain each other
      for (j <- obstacles.indices; p = obstacles(j); if i != j) {
        val inter = o.intersectPolygon(p)

        if (inter) throw ContestException(OBSTACLES_INTERSECT)

        if (o.containsCell(p.vertices.head))
          throw ContestException(OBSTACLES_INTERSECT)

        if (p.containsCell(o.vertices.head))
          throw ContestException(OBSTACLES_INTERSECT)
      }
    }

    // Check boosters
    for (i <- boosterPositions.indices;
         (_, bpos) = boosterPositions(i)) {

      if (!room.containsCell(bpos))
        throw ContestException(BOOSTER_NOT_IN_ROOM)

      if (obstacles.exists(_.containsCell(bpos)))
        throw ContestException(BOOSTER_IN_OBSTACLE)

      for (j <- boosterPositions.indices;
           (_, cpos) = boosterPositions(j); if i != j) {
        if (cpos == bpos)
          throw ContestException(BOOSTER_CLASH)
      }
    }

    true
  }


  def roomWithinPositiveBoundingBox(room: IPolygon): Boolean = {
    val ((xl, yl), (xr, yr)) = room.boundingBox

    if (!(xl >= 0 && yl >= 0 && xl < xr && yl < yr)) return false

    room.vertices.forall { case IPoint(x, y) =>
      xl <= x && x <= xr &&
        yl <= y && y <= yr
    }
  }

  private def checkObstacle(o: IPolygon): Boolean = {
    if (!o.isWellFormed) return false
    if (!o.isRectilinear) return false
    true
  }


}
