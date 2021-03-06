package lambda.contest.checkers

import lambda.contest.ContestErrorMessages._
import lambda.contest.{Booster, ContestException, ContestTask}
import lambda.geometry.integer.{IPoint, IPolygon}

/**
  * @author Ilya Sergey
  */
object ContestTaskUtils {

  /**
    * Assuming the room always has at least one cell
    */
  def getVacantCell(task: ContestTask): IPoint = {
    val ContestTask(room, initPos, obstacles, boosterPositions) = task

    val c1 = (p: IPoint) => obstacles.forall(o => !o.containsCell(p))
    val c2 = (p: IPoint) => initPos != p
    val c3 = (p: IPoint) => boosterPositions.forall { case (_, b) => b != p }

    var pt = room.randomCellWithin
    while (!c1(pt) || !c2(pt) || !c3(pt)) {
      pt = room.randomCellWithin
    }
    pt
  }

  /**
    * Assuming such cells exist 
    */
  def getVacantCellNotTouchingWalls(task: ContestTask): IPoint = {
    val ContestTask(room, initPos, obstacles, boosterPositions) = task

    val c1 = (p: IPoint) => obstacles.forall(o => !o.containsCell(p) && !o.intersectPolygon(p.toSquare))
    val c2 = (p: IPoint) => initPos != p
    val c3 = (p: IPoint) => boosterPositions.forall { case (_, b) => b != p }
    val c4 = (p: IPoint) => room.containsPolygonProperly(p.toSquare)

    var pt = room.randomCellWithin
    while (!(c1(pt)
      && c2(pt)
      && c3(pt)
      && c4(pt)
      )) {
      pt = room.randomCellWithin
    }
    pt
  }

  /**
    * Generate a random box within the task room 
    */
  def findRandomBox(task: ContestTask, minSize: Int = 5): Option[(IPoint, IPolygon)] = {
    val (dx, dy) = task.room.dimensions
    val attempts = math.max(dx, dy)
    
    val ContestTask(room, initPos, obstacles, boosterPositions) = task

    def boxIsAllowed(xl: Int, yl: Int, xr: Int, yr: Int): Boolean = {
      val rectangle = IPolygon(List(IPoint(xl, yl), IPoint(xr, yl), IPoint(xr, yr), IPoint(xl, yr)))
      val c1 = room.containsPolygonProperly(rectangle)
      val c2 = obstacles.forall(o => !o.intersectPolygon(rectangle))
      val c3 = !rectangle.containsCell(initPos)
      val c4 = boosterPositions.forall { case (_, b) => !rectangle.containsCell(b) }
      c1 && c2 && c3 && c4
    }

    def expandBoxFromPoint(pt: IPoint): IPolygon = {
      var xl = pt.x
      var xr = pt.x + 1
      var yl = pt.y
      var yr = pt.y + 1
      var xlb, xrb, ylb, yrb = true
      assert(boxIsAllowed(xl, yl, xr, yr))
      while (xlb || xrb || ylb || yrb) {
        // Expand left
        if (xlb && boxIsAllowed(xl - 1, yl, xr, yr)) {
          xl = xl - 1
        } else {
          xlb = false
        }
        // Expand down
        if (ylb && boxIsAllowed(xl, yl - 1, xr, yr)) {
          yl = yl - 1
        } else {
          ylb = false
        }
        // Expand right
        if (xrb && boxIsAllowed(xl, yl, xr + 1, yr)) {
          xr = xr + 1
        } else {
          xrb = false
        }
        // Expand up
        if (yrb && boxIsAllowed(xl, yl, xr, yr + 1)) {
          yr = yr + 1
        } else {
          yrb = false
        }
      }
      IPolygon(List(IPoint(xl, yl), IPoint(xr, yl), IPoint(xr, yr), IPoint(xl, yr)))
    }

    def tryWithPoint(pt: IPoint): Option[IPolygon] = {
      val poly = expandBoxFromPoint(pt)
      val (dx, dy) = poly.dimensions
      if (math.min(dx, dy) < minSize) {
        None
      } else Some(poly)
    }

    for (_ <- 0 until attempts) {
      val pt = getVacantCellNotTouchingWalls(task)
      tryWithPoint(pt) match {
        case Some(poly) =>
          return Some(poly.getMinXY, poly)
        case _ => // continue
      }
    }
    None
  }


  /**
    * Check if the task is well-formed
    */
  def checkTaskWellFormed(task: ContestTask): Boolean = {

    val ContestTask(room, initPos, obstacles, boosterPositions) = task

    // Check room
    if (!room.isWellFormed) throw ContestException(MALFORMED_ROOM, None)
    if (!room.isRectilinear) throw ContestException(NON_RECTILINEAR_ROOM, None)
    if (!room.containsCell(initPos)) throw ContestException(BAD_INIT_POS, None)
    if (!roomWithinPositiveBoundingBox(room)) throw ContestException(BOUNDING_BOX, None)

    // Check obstacles
    for (i <- obstacles.indices; o = obstacles(i)) {

      if (!checkObstacle(o))
        throw ContestException(MALFORMED_OBSTACLE, None)

      if (o.containsCell(initPos))
        throw ContestException(INIT_POS_OBSTACLE, None)

      if (!room.containsPolygonProperly(o)) {
        throw ContestException(OBSTACLE_NOW_WITHIN, None)
      }
      
      // Obstacles do not intersect or contain each other
      for (j <- obstacles.indices; p = obstacles(j); if i != j) {
        val inter = o.intersectPolygon(p)

        if (inter) throw ContestException(OBSTACLES_INTERSECT, None)

        if (o.containsCell(p.vertices.head))
          throw ContestException(OBSTACLES_INTERSECT, None)

        if (p.containsCell(o.vertices.head))
          throw ContestException(OBSTACLES_INTERSECT, None)
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

    if (!(xl >= 0 && yl >= 0 && xl < xr && yl < yr)) {
      return false
    }

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
