package lambda.misc.artgallery

import lambda.util.SolutionReportUtils

/**
  * @author Ilya Sergey
  */

trait ArtGallerySolutionReportUtils extends SolutionReportUtils {

  def reportOutOfRangeErrors(oor: Seq[Int]): String =
    s"""
      |Unfortunately, your input file contained the following out-of range entries (there are no such polygon IDs in the task input):
      |
      |${oor.mkString(", ")}
      |
      |Please, check your solution file.
      |""".stripMargin
}
