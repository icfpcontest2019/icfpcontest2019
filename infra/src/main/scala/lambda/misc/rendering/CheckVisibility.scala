package lambda.misc.rendering

import java.io.File

import lambda.geometry.floating.FPoint
import lambda.geometry.floating.visibility.VisibilityChecker
import lambda.geometry.floating.visibility.VisibilityChecker.FailReason
import lambda.geometry.floating.visibility.VisibilityChecker.FailReason.FailReason
import lambda.misc.artgallery.CheckInputParser

import scala.io.Source

/**
  * @author Ilya Sergey
  */

object CheckVisibility {

  val ansi_GREEN = "\u001B[32m"
  val ansi_RED = "\u001B[31m"
  val ansi_RESET = "\u001B[0m"

  def main(args: Array[String]) {
    val inputPath =
      if (args.length > 0) args(0)
      else Seq(System.getProperty("user.dir"), "input.pol").mkString(File.separator)
    val lines = Source.fromFile(inputPath).getLines

    val pResults = lines.toSeq.map(CheckInputParser(_))
    if (pResults.exists(!_.successful)) {
      for (r <- pResults; if !r.successful) {
        System.err.println(r.toString)
      }
      return
    }

    val inputs = pResults.map(_.get).sortBy(_._1)

    for ((num, (pol, guards)) <- inputs) {
      val (b, reason, _) = VisibilityChecker.checkVisibility(pol, guards)
      val out =
      if (b) s"${ansi_GREEN}Polygon $num: OK$ansi_RESET" else {
        s"${ansi_RED}Polygon $num: ${renderReason(reason)}$ansi_RESET"
      }
      println(out)
    }
  }

  def renderReason(reason: Option[(FPoint, FailReason)]): String = reason match {
    case Some((p, r)) => r match {
      case FailReason.Outside => s"Point $p is outside of the polygon"
      case FailReason.NotVisible => s"Point $p is not visible for the set of guards"
    }
    case None => "OK"
  }
}
