package lambda.solvers

import lambda.contest.ContestException

/**
  * @author Ilya Sergey
  */


trait AStarGraph[State] {
  def cost(src: State, dst: State): Int

  def goal: State

  def getNextStates(state: State): List[State]
}


class AStar[State] {

  type Problem = AStarGraph[State]


  case class Path(costFromStart: Int, totalCost: Int, head: State, tail: List[State])

  def createPath(from: Option[Path], problem: Problem, state: State): Path = {
    val (costFromStart, tail) = from match {
      case None => (0, Nil)
      case Some(p) =>
        (p.costFromStart + problem.cost(p.head, state), p.head :: p.tail)
    }
    val totalCost = costFromStart + problem.cost(state, problem.goal)
    Path(costFromStart, totalCost, state, tail)
  }

  /*
  Returns true if path p is better than path q
   */
  def better(p: Path, q: Path): Boolean = p.totalCost < q.totalCost

  /* [pickup_eq_path p l] returns [Some (q, l')] where [q] is the path that
      indicates the same position as [p] and [l] is a list excluding [q]. 
   */

  def pickupEqPath(p: Path, l: List[Path]): Option[(Path, List[Path])] = l.partition(q => q.head == p.head) match {
    case (Nil, _) => None
    case (List(q), l1) => Some((q, l1))
    case _ => throw ContestException("duplicated paths in open/close list")
  }

  /*
  [trace_next_states problem open_list close_list path] traces the next
   states of [path.head] and return [(open_list', close_list')] where [open_list'] and [close_list']
   are respectively an open list and a close list after all of the next
   states are traced. 
   */

  def traceNextStates(problem: Problem, ol0: List[Path], cl0: List[Path], path0: Path): (List[Path], List[Path]) = {
    def traceState(ol_cl: (List[Path], List[Path]), state: State) = {
      val (ol: List[Path], cl: List[Path]) = ol_cl
      val path = createPath(Some(path0), problem, state)
      pickupEqPath(path, ol) match {
        case Some((q, ol1)) => if (better(path, q)) (path :: ol1, cl) else (ol, cl)
        case None => pickupEqPath(path, cl) match {
          case Some((q, cl1)) => if (better(path, q)) (path :: ol, cl1) else (ol, cl)
          case None => (path :: ol, cl)
        }
      }
    }

    problem.getNextStates(path0.head).foldLeft((ol0, cl0))(traceState)
  }

  /*
  [pickup_best_path l] returns [Some (p, l')] where [p] is the path that
   has the least cost in [l] and [l'] is an open list without [p].
  */

  def pickupBestPath(ps: List[Path]): Option[(Path, List[Path])] = ps match {
    case Nil => None
    case h :: t =>
      def aux(yl: (Path, List[Path]), x: Path): (Path, List[Path]) = {
        val (y: Path, l: List[Path]) = yl
        if (better(y, x)) (y, x :: l) else (x, y :: l)
      }

      Some(t.foldLeft((h, Nil: List[Path]))(aux))
  }

  def search(problem: Problem, start: State): List[State] = {
    def aux(ol: List[Path], cl: List[Path]): Option[Path] = {
      pickupBestPath(ol) match {
        case None => None
        case Some((p, ol1)) =>
          if (p.head == problem.goal) Some(p)
          else {
            val ol_cl = traceNextStates(problem, ol1, p :: cl, p)
            aux(ol_cl._1, ol_cl._2)
          }
      }
    }

    aux(List(createPath(None, problem, start)), Nil) match {
      case None => throw ContestException("Path not found")
      case Some(p) => p.head :: p.tail
    }
  }

}
