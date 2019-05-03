package lambda.contest

import lambda.contest.ContestConstants._
import lambda.contest.ErrorMessages._
import lambda.geometry.integer.{IPoint, IPolygon}

/**
  * @author Ilya Sergey
  */


/* ********************************************** */
//          Bare contest task - as given          //
/* ********************************************** */

case class ContestTask(room: IPolygon,
                       initPos: IPoint,
                       obstacles: List[IPolygon],
                       boosters: List[(Booster.Value, IPoint)])


/* ********************************************** */
//          Room cell and its properties          //
/* ********************************************** */

object Booster extends Enumeration {
  type Booster = Value

  val BatteriesBooster, CoffeeBooster, DrillBooster,
  TeleportBooster, CallWatchmanBooster, CallPoint = Value

}

/**
  * Representation of a room's cell
  *
  * @param hasSpace         true when not part of the wall
  * @param illuminated      true when illluminated by a watchman
  * @param boosterToCollect contains (at most one) optional booster to collect
  * @param callPoint        true if has a call point to summon another watchman
  * @param teleport         true if has a teleport installed
  */

class Cell(private var hasSpace: Boolean = false,
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


/* ********************************************** */
//            Watchman and its properties         //
/* ********************************************** */

import scala.collection.mutable.{Set => MSet}

abstract class ActiveBooster(private var timeLeft: Int) {
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

class CoffeeBooster extends ActiveBooster(COFFEE_TIME) {
  def toBoosterTag = Booster.CoffeeBooster
}

class DrillBooster extends ActiveBooster(DRILL_TIME) {
  def toBoosterTag = Booster.DrillBooster
}


class Watchman {

  // The torch can grow
  private val torch: MSet[(Int, Int)] = MSet(DEFAULT_TORCH: _*)

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
    * Return the cells that are lit by the torch (including the )
    */
  def getTorchRange(pos: IPoint) = {
    val torchCells = torch.toList.map { case (x, y) => IPoint(pos.x + x, pos.y + y) }
    pos :: torchCells
  }

  /* ********************************************** */
  //            Working with active boosters        //
  /* ********************************************** */

  // The boosters that will expire
  private var activeBoosters: MSet[ActiveBooster] = MSet()

  // Return active boosters
  def getActiveBoosters: List[Booster.Value] =
    activeBoosters.toList.map(_.toBoosterTag).distinct

  // Removes expired boosters
  def decrementBoosters(): Unit = {
    val boosters = activeBoosters.toList
    activeBoosters.clear()
    boosters.foreach(_.decrementTimeLeft())
    boosters.filter(_.getRemainingTime <= 0).foreach(
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

  /* ********************************************** */
  //              Working with portals              //
  /* ********************************************** */

  // A pending installed portal awaiting to install a pair for
  private var pendingPortal: Option[IPoint] = None

  def installOrLinkPortal(cell: IPoint) = pendingPortal match {
    // Install a new portal without a pair
    case None =>
      pendingPortal = Some(cell)
      None
    // Installing a paired portal: return coordinates of an old one
    case Some(old) =>
      pendingPortal = None
      Some(old)
  }

}


