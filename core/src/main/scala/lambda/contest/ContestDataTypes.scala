package lambda.contest

import lambda.contest.ContestConstants._
import lambda.contest.ContestErrorMessages._
import lambda.geometry.integer.IPointUtils._
import lambda.geometry.integer.{IPoint, IPointUtils, IPolygon}

/**
  * @author Ilya Sergey
  */


/* ********************************************** */
//          Bare contest task - as given          //
/* ********************************************** */

case class ContestTask(room: IPolygon,
                       initPos: IPoint,
                       obstacles: List[IPolygon],
                       boosters: List[(Booster.Value, IPoint)]) {
  override def toString = {
    val roomStr = room.vertices.map(_.toString).mkString(",")
    val posStr = initPos.toString
    val obStr = obstacles.map(obs => obs.vertices.map(_.toString).mkString(",")).mkString(";")
    val booStr = boosters.map { case (b, p) => s"${Booster.toChar(b)}${p.toString}" }.mkString(";")
    List(roomStr, posStr, obStr, booStr).mkString("#")
  }

}


/* ********************************************** */
//                    Boosters                    //
/* ********************************************** */

object Booster extends Enumeration {
  type Booster = Value

  val BatteriesBooster, CoffeeBooster, DrillBooster,
  TeleportBooster, CallWatchmanBooster, CallPoint = Value

  def toChar(b: Value): Char = b match {
    case BatteriesBooster => BATTERIES_LETTER
    case CoffeeBooster => COFFEE_LETTER
    case DrillBooster => DRILL_LETTER
    case TeleportBooster => INSTALL_TELEPORT_LETTER
    case CallWatchmanBooster => CALL_FRIEND_LETTER
    case CallPoint => CALL_POINT_LETTER
  }

  def pp(b: Value): String = b match {
    case BatteriesBooster => "Batteries"
    case CoffeeBooster => "Coffee"
    case DrillBooster => "Drill"
    case TeleportBooster => "Teleport"
    case CallWatchmanBooster => "Fork"
    case CallPoint => "CallPoint"
  }
  
}

/* ********************************************** */
//          Room cell and its properties          //
/* ********************************************** */

/**
  * Representation of a room's cell
  *
  * @param hasSpace         true when not part of the wall
  * @param illuminated      true when illuminated by a watchman
  * @param boosterToCollect contains (at most one) optional booster to collect
  * @param callPoint        true if has a call point to summon another watchman
  * @param teleport         true if has a teleport installed
  */

case class Cell(private var hasSpace: Boolean = false,
                private var illuminated: Boolean = false,
                private var boosterToCollect: Option[Booster.Value] = None,
                private var callPoint: Boolean = false,
                private var teleport: Boolean = false) {

  /**
    * Avoiding checks with laziness
    */
  def doIfHasSpace[T](act: => T): T = {
    if (!canStep) throw ContestException(CELL_DOES_NOT_HAVE_SPACE)
    act
  }

  /* ------------------------------------------- */
  //               Properties                    //
  /* ------------------------------------------- */

  def canStep: Boolean = hasSpace

  def isIlluminated: Boolean = illuminated

  def hasCallPoint: Boolean = callPoint

  def hasTeleport: Boolean = teleport

  def peekBooster: Option[Booster.Value] = boosterToCollect

  def vacant: Boolean =
    !(teleport || callPoint) && boosterToCollect.isEmpty

  /* ------------------------------------------- */
  //               Set-up actions                //
  /* ------------------------------------------- */

  def clearSpace(): Unit = {
    hasSpace = true
  }

  def setCallPoint(): Boolean =
    if (hasSpace && vacant) {
      callPoint = true
      true
    } else false

  def setBooster(b: Booster.Value) = {
    if (hasSpace && vacant) {
      boosterToCollect = Some(b)
      true
    } else false
  }

  /* ------------------------------------------- */
  //               In-game actions               //
  /* ------------------------------------------- */

  def shedLight(): Unit = doIfHasSpace {
    illuminated = true
  }

  /**
    * As a side effect removes booster from the cell
    */
  def collectBooster(): Option[Booster.Value] = doIfHasSpace {
    boosterToCollect match {
      case Some(b) =>
        boosterToCollect = None
        Some(b)
      case None => None
    }
  }

  def installNewTeleport(): Unit = doIfHasSpace {
    if (teleport) throw ContestException(TELEPORT_INSTALLED)
    if (callPoint) throw ContestException(CALL_POINT_INSTALLED)
    teleport = true
  }

}


import scala.collection.mutable.{Set => MSet}

/* ********************************************** */
//            Watchmen and their properties       //
/* ********************************************** */


class Watchman(private val torch: MSet[(Int, Int)] =
               MSet(DEFAULT_CONTEST_TORCH: _*)) {

  // Torch rotations
  def rotateTorchLeft(): Unit = {
    val cells = torch.toList
    torch.clear()
    cells.foreach { case (x, y) =>
      val p = IPoint(x, y).rotateLeft
      torch.add((p.x, p.y))
    }
  }

  def rotateTorchRight(): Unit = {
    val cells = torch.toList
    torch.clear()
    cells.foreach { case (x, y) =>
      val p = IPoint(x, y).rotateRight
      torch.add((p.x, p.y))
    }
  }

  /**
    * Return the cells that are lit by the torch (including the beginning)
    */
  def getTorchRange(pos: IPoint): List[IPoint] = {
    val torchCells = torch.toList.map { case (x, y) => IPoint(pos.x + x, pos.y + y) }
    pos :: torchCells
  }

  /* ---------------------------------------------- */
  //            Working with active boosters        //
  /* ---------------------------------------------- */

  // The boosters that will expire
  private var activeBoosters: MSet[ActiveBooster] = MSet()

  // Return active boosters
  private def getActiveBoosters: List[Booster.Value] =
    activeBoosters.toList.map(_.toBoosterTag).distinct

  // Return active boosters with remaining time
  def getActiveBoostersWithTime: List[(Booster.Value, Int)] =
    activeBoosters.toList.map(b => (b.toBoosterTag, b.getRemainingTime)).distinct

  def isUnderCoffe: Boolean = getActiveBoosters.contains(Booster.CoffeeBooster)

  def isDrillGuy: Boolean = getActiveBoosters.contains(Booster.DrillBooster)


  // Removes expired boosters
  def decrementActiveBoosters(): Unit = {
    val boosters = activeBoosters.toList
    activeBoosters.clear()
    boosters.foreach(_.decrementTimeLeft())
    boosters.filter(_.getRemainingTime >= 0).foreach(
      b => activeBoosters.add(b))
  }

  def addActiveBooster(newBooster: ActiveBooster): Unit = {
    val boosters = activeBoosters.toList
    activeBoosters.clear()
    if (!boosters.map(_.toBoosterTag).contains(newBooster.toBoosterTag)) {
      // A new booster
      for (elem <- newBooster :: boosters) {
        activeBoosters.add(elem)
      }
    } else {
      // Update booster times
      boosters.foreach { b =>
        b += newBooster
        activeBoosters.add(b)
      }
    }
  }

  /* ---------------------------------------------- */
  //            Adding a new battery                //
  /* ---------------------------------------------- */

  def addBattery(dx: Int, dy: Int) = {
    val squares = getTorchRange(IPoint(0, 0)).map(_.toSquare)
    val newSquare = IPoint(dx, dy).toSquare
    if (!squareTouchesOtherSquares(newSquare, squares)) {
      throw ContestException(BAD_BATTERY_POSITION)
    }
    torch.add((dx, dy))
  }

}

/* ********************************************** */
//            Boosters applied to watchmen        //
/* ********************************************** */

abstract sealed class ActiveBooster(private var timeLeft: Int) {
  def decrementTimeLeft(): Unit = {
    timeLeft = timeLeft - 1
  }

  def +=(other: ActiveBooster): Unit = {
    if (other.toBoosterTag == this.toBoosterTag) {
      timeLeft = timeLeft + other.timeLeft
    }
  }

  def getRemainingTime: Int = timeLeft

  def toBoosterTag: Booster.Value
}

class ActiveCoffeeBooster extends ActiveBooster(COFFEE_TIME) {
  def toBoosterTag = Booster.CoffeeBooster
}

class ActiveDrillBooster extends ActiveBooster(DRILL_TIME) {
  def toBoosterTag = Booster.DrillBooster
}
