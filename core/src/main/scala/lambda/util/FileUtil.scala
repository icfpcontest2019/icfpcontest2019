package lambda.util

import java.io.{BufferedWriter, File, FileWriter}

import scala.io.Source

/**
  * @author Ilya Sergey
  */
object FileUtil {

  def writeToNewFile(fpath: String, text: String): Unit = {
    val file = new File(fpath)
    if (file.exists()) {
      file.delete()
    }
    if (!file.exists()) {
      file.createNewFile()
    }
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(text)
    bw.close()
  }

  def readFromFile(fpath: String) : List[String] = {
    try {
      val bufferedSource = Source.fromFile(fpath)
      val res = bufferedSource.getLines.toList
      bufferedSource.close
      res
    } catch {
      case _: Throwable => Nil
    }
  }

  def readFromFileWithNewLines(fpath: String) : String = {
    try {
      val bufferedSource = Source.fromFile(fpath)
      val res = bufferedSource.getLines.toList.mkString("\n")
      bufferedSource.close
      res
    } catch {
      case _: Throwable => ""
    }
  }

  def intAs3CharString(k: Int): String = f"$k%03d"

}
