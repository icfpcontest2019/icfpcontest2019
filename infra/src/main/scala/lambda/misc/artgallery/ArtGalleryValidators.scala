package lambda.misc.artgallery

import java.util.Date

import akka.actor.ActorRef
import lambda.geometry.floating.FPoint
import lambda.geometry.floating.visibility.VisibilityChecker
import lambda.misc.artgallery.ArtGalleryInstance.{CheckSubmission, GuardSubmission, _}
import lambda.util.project.ProjectConstants._
import lambda.util.project.{BadSetupException, ScenarioInstance}
import lambda.util.{ScenarioMessage, SolutionValidator}


case class GuardsSolution(override val lines: Seq[String]) extends ScenarioMessage(lines) {
  override def getValidator(sRoot: String, tname: String, logActor: ActorRef) =
    new GuardsSolutionValidator(sRoot, tname, logActor)
  override def toSubmissionType = GuardSubmission
}

case class CheckSolution(override val lines: Seq[String]) extends ScenarioMessage(lines) {

  override def getValidator(sRoot: String, tname: String, logActor: ActorRef) =
    new CheckSolutionValidator(sRoot, tname, logActor)
  override def toSubmissionType = CheckSubmission
}

class GuardsSolutionValidator(override val sRoot: String,
                              override val tname: String,
                              override val logActor: ActorRef)
    extends SolutionValidator with ArtGallerySolutionReportUtils {

  type GuardsSolutionForPol = (Int, Seq[FPoint])
  type GuardsFailureWithReason = (Int, String)
  type OkSolutionType = (Int, Seq[FPoint], Boolean, Int)
  type GuardsOkOrReason = Either[OkSolutionType, GuardsFailureWithReason]

  val scenarioInstance: ScenarioInstance = ArtGalleryInstance

  def processSolution(lines: Seq[String]): (String, Option[Seq[OkSolutionType]]) = {
    val preSols = prepareGuardsSolutions(lines)
    val parseErrors = preSols.filter(_.isRight)

    // Check for parsing errors
    if (parseErrors.nonEmpty)
      return (reportParseErrors(parseErrors.map { case Right(s) => s; case _ => "" }), None)

    // Check for duplicating entries
    val entries = for (ps <- preSols if ps.isLeft; Left(a) = ps) yield a
    if (containsDuplicates(entries)) return (reportDuplicatesError(), None)

    // Check for out-of range polygons
    val oor = outOfRangeGuardResults(entries)
    if (oor.nonEmpty) return (reportOutOfRangeErrors(oor), None)

    // Now, we're checking the validity of the solution
    val (succ, fail) = try {
      validateGuardProblemSolution(entries)
    } catch {
      case e: Throwable => throw BadSetupException(e.getMessage + "\n" + e.getStackTrace.mkString("\n"))
    }
    (reportGuardOutcomes(succ, fail), Some(succ))
  }

  def validateGuardProblemSolution(es: Seq[GuardsSolutionForPol]): (Seq[OkSolutionType], Seq[GuardsFailureWithReason]) = {
    val outcomes = es.map(validateGuardProblemSolutionEntry)
    val succ = for (o <- outcomes; if o.isLeft; Left(r) = o) yield r
    val fail = for (o <- outcomes; if o.isRight; Right(r) = o) yield r
    (succ, fail)
  }

  /**
    * Left  --> OK
    * Right --> Nope
    */
  def validateGuardProblemSolutionEntry(e: GuardsSolutionForPol): GuardsOkOrReason = {
    val (pNum, guards) = e
    val pres = retrieveInputForGuards(sRoot, pNum)
    if (pres.isRight) return Right(pNum, s"No polygon found for the ID $pNum")
    // In fact, this shouldn't happen
    val Left(pol) = pres

    // Input is too large
    val sz = guards.size
    val pvs = pol.vertices.size
    if (sz * 3 > pvs + 1) {
      return Right(pNum, s"You solution for polygon $pNum is too large: you have provided $sz guards, whereas ${pvs / 3} should suffice.")
    }

    val guards1 = guards.distinct.map(epsMove(pol.vertices))

    val (b, reason, _) = VisibilityChecker.checkVisibility(pol, guards1)
    if (b) {
      val isOk = guards.size <= pol.vertices.size / 3
      Left((pNum, guards, isOk, pol.vertices.size))
    }
    else if (reason.isDefined) {
      val (pt, fr) = reason.get
      val msg = VisibilityChecker.toStringWithPoint(fr, pt)
      Right(pNum, msg)
    } else Right(pNum, "")
  }

  private def containsDuplicates(preEntries: Seq[GuardsSolutionForPol]): Boolean = {
    val s = preEntries.map(_._1)
    s.distinct.size != s.size
  }

  private def outOfRangeGuardResults(preEntries: Seq[GuardsSolutionForPol]): Seq[Int] = {
    val s = preEntries.map(_._1)
    val inputPols = getGuardsPolygonMap(sRoot)
    val ook = s.filterNot(inputPols.keySet.contains)
    ook
  }

  private def prepareGuardsSolutions(s: Seq[String]): Seq[Either[GuardsSolutionForPol, String]] = {
    import GuardSolutionParser._
    s.map { l =>
      val res: ParseResult[GuardsSolutionForPol] = GuardSolutionParser(l)
      if (res.successful) Left(res.get) else Right(res.toString)
    }
  }

  private def reportGuardOutcomes(succ: Seq[OkSolutionType], fail: Seq[GuardsFailureWithReason]): String = {
    val succNotOk = succ.filter(!_._3)
    val totalSize = ArtGalleryInstance.getGuardsPolygonMap(sRoot).size

    if (fail.isEmpty) {
      if (succNotOk.isEmpty) {
        if (fail.isEmpty && succ.size == totalSize) {
          "Congratulations, your solution has been fully accepted!\nCheck the score table to know your place in the rating."
        } else {
          "Your solution has been accepted, however, it is incomplete and didn't provide solutions for all components of the input.\nCheck the score table to see the missing pieces."
        }
      } else {
        s"""
           |Your solution passes the check, but for some of the polygons, the guards sets are suboptimal and will not be accepted:
           |
           |${succNotOk.map { case (a, b, _, c) => s"For polygon $a (with $c vertices), less than ${b.size} guards should suffice." }.mkString("\n")}
         """.stripMargin
      }
    } else {
      s"""
        |There are some problems with the following parts of your solution:
        |
        |${fail.map { case (p, r) => s"For polygon $p: $r" }.mkString("\n")}
        |
        |The remaining successful parts of your solution are recorded, and you can check them in the score table.
      """.stripMargin
    }
  }

  def dumpResultsToFiles(submissionTime: Date,
                         succOpt: Option[Seq[OkSolutionType]],
                         lines: Seq[String], msg: String): Unit = {
    dumpOkGuardsResults(submissionTime, succOpt)
    dumpMessage(GuardSubmission, msg, submissionTime)

  }
  private def dumpOkGuardsResults(time: Date, succOpt: Option[Seq[OkSolutionType]]): Unit = {
    //if (succOpt.isEmpty) return

    val date = dateFormat.format(time)
    // only take OK solutions
    val succ = (if (succOpt.isEmpty) Seq.empty else succOpt.get).filter(_._3)

    // Dump in the format:
    // pID : guardsSize / numOfVertices
    val lines = succ.map { case (pNum, pGuards, _, pN) => s"$pNum : ${pGuards.size} / $pN" }.sortWith(_ <= _)
    val out = (Seq(date) ++ lines).mkString("\n")

    writeIntoResultFile(sRoot, tname, GuardSubmission, out, resultsPrefix)
  }
}

class CheckSolutionValidator(override val sRoot: String,
                             override val tname: String,
                             override val logActor: ActorRef)
    extends SolutionValidator with ArtGallerySolutionReportUtils {

  val scenarioInstance: ScenarioInstance = ArtGalleryInstance

  type OkSolutionType = (Int, FPoint)
  type ACheckSolution = (Int, FPoint)
  type CheckFailureWithReason = (Int, String)
  type CheckOkOrReason = Either[OkSolutionType, CheckFailureWithReason]

  def processSolution(lines: Seq[String]): (String, Option[Seq[OkSolutionType]]) = {
    val preSols = prepareCheckSolutions(lines)
    val parseErrors = preSols.filter(_.isRight)

    // Check for parsing errors
    if (parseErrors.nonEmpty)
      return (reportParseErrors(parseErrors.map { case Right(s) => s; case _ => "" }), None)

    // Check for duplicating entries
    val entries = for (ps <- preSols if ps.isLeft; Left(a) = ps) yield a
    if (containsDuplicates(entries)) return (reportDuplicatesError(), None)

    // Check for out-of range polygons
    val oor = outOfRangeCheckResults(entries)
    if (oor.nonEmpty) return (reportOutOfRangeErrors(oor), None)

    // Now, we're checking the validity of the solution
    val (succ, fail) = try {
      validateCheckProblemSolution(entries)
    } catch {
      case e: Throwable => throw BadSetupException(e.getMessage + "\n" + e.getStackTrace.mkString("\n"))
    }
    (reportCheckOutcomes(succ, fail), Some(succ))
  }

  private def prepareCheckSolutions(s: Seq[String]): Seq[Either[ACheckSolution, String]] = {
    import CheckSolutionParser._
    s.map { l =>
      val res: ParseResult[ACheckSolution] = CheckSolutionParser(l)
      if (res.successful) Left(res.get) else Right(res.toString)
    }
  }

  private def containsDuplicates(preEntries: Seq[ACheckSolution]): Boolean = {
    val s = preEntries.map(_._1)
    s.distinct.size != s.size
  }

  private def outOfRangeCheckResults(preEntries: Seq[ACheckSolution]): Seq[Int] = {
    val s = preEntries.map(_._1)
    val inputPols = getGuardsPolygonMap(sRoot)
    val ook = s.filterNot(inputPols.keySet.contains)
    ook
  }

  private def validateCheckProblemSolution(es: Seq[ACheckSolution]): (Seq[OkSolutionType], Seq[CheckFailureWithReason]) = {
    val outcomes = es.map(validateCheckProblemSolutionEntry)
    val succ = for (o <- outcomes; if o.isLeft; Left(r) = o) yield r
    val fail = for (o <- outcomes; if o.isRight; Right(r) = o) yield r
    (succ, fail)
  }

  /**
    * Left  --> OK
    * Right --> Nope
    */
  private def validateCheckProblemSolutionEntry(e: ACheckSolution): CheckOkOrReason = {
    val (pNum, thief) = e
    val pres = retrieveInputForCheck(sRoot, pNum)
    if (pres.isRight) return Right(pNum, s"No polygon found for the ID $pNum")
    // In fact, this shouldn't happen
    val Left((pol, guards)) = pres

    val (b, reason) = VisibilityChecker.isInvisible(pol, guards, thief)
    if (b) Left((pNum, thief)) else Right(pNum, reason)
  }

  private def reportCheckOutcomes(succ: Seq[OkSolutionType], fail: Seq[CheckFailureWithReason]): String = {
    val totalSize = ArtGalleryInstance.getProblemMap(sRoot).size

    if (fail.isEmpty && succ.size == totalSize) {
      "Congratulations, your solution has been fully accepted!\nCheck the score table to know how your competition is doing. :)"
    } else if (fail.isEmpty) {
      "Your solution has been accepted, however, it is incomplete and didn't provide solutions for all components of the input.\nCheck the score table to see the missing pieces."
    } else {
      s"""
         |There are some problems with the following parts of your solution:
         |
        |${fail.map { case (p, r) => s"For polygon $p: $r" }.mkString("\n")}
         |
        |${if (succ.nonEmpty) "The remaining successful parts of your solution are recorded, and you can check them in the score table." else ""}
      """.stripMargin
    }
  }

  def dumpResultsToFiles(submissionTime: Date,
                         succOpt: Option[Seq[OkSolutionType]],
                         lines: Seq[String], msg: String): Unit = {
    dumpOkCheckResults(submissionTime, succOpt)
    dumpMessage(CheckSubmission, msg, submissionTime)
  }

  private def dumpOkCheckResults(time: Date, succOpt: Option[Seq[OkSolutionType]]): Unit = {
    //if (succOpt.isEmpty) return

    val date = dateFormat.format(time)
    // only take OK solutions
    val succ = if (succOpt.isEmpty) Seq.empty else succOpt.get

    // Dump in the format:
    // pID : guardsSize / numOfVertices
    val lines = succ.map { case (pNum, thief) => s"$pNum : $thief" }.sortWith(_ <= _)
    val out = (Seq(date) ++ lines).mkString("\n")

    writeIntoResultFile(sRoot, tname, CheckSubmission, out, resultsPrefix)
  }

}
