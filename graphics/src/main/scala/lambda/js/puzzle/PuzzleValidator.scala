package lambda.js.puzzle

import lambda.contest.ContestException
import lambda.contest.blockchain.PuzzleCheckingUtils.parsePuzzleLine
import lambda.contest.blockchain.{BlockPuzzle, PuzzleCheckingUtils}
import lambda.js.JSGrading
import org.scalajs.dom
import org.scalajs.dom.raw.{Blob, HTMLInputElement}
import org.scalajs.dom._

/**
  * @author Ilya Sergey
  */
object PuzzleValidator extends JSGrading {

  lazy val puzzleFileInput = document.getElementById(submitTaskId).asInstanceOf[HTMLInputElement]
  lazy val puzzleSolutionFileInput = document.getElementById(submitSolutionId).asInstanceOf[HTMLInputElement]

  var currentPuzzleText: String = ""
  var currentPuzzleSolutionText: String = ""
  var currentPuzzle : Option[BlockPuzzle] = None

  def main(): Unit = {
    val centered = document.getElementById("main_section")
    mkFileInput(centered, submitTaskId, SUBMIT_PUZZLE_TEXT)
    mkFileInput(centered, submitSolutionId, SUBMIT_TASK_TEXT)
    mkButton(centered, execButtonId, CHECK_TEXT)
    mkTextField(centered, outTextFieldId)
    puzzleFileInput.onchange = puzzleHandler
    puzzleSolutionFileInput.onchange = puzzleSolutionHandler
    execButton.onclick = execHandler
  }

  private def clearPuzzle(): Unit = {
    currentPuzzleText = ""
  }

  private def clearPuzzleSolution(): Unit = {
    currentPuzzleSolutionText = ""
    currentTask = None
  }

  /* ---------------------------------------------------------------- */
  /*                          Solution checking                       */
  /* ---------------------------------------------------------------- */


  private val puzzleHandler: Function1[Event, Unit] = event => {
    clearPuzzle()
    if (!puzzleFileInput.files(0).isInstanceOf[Blob]) {
      setText(NO_PUZZLE_FILE)
    } else {
      val puzzleReader = new FileReader()
      puzzleReader.onloadend = _ => {
        val text = puzzleReader.result.toString
        if (text == currentPuzzleText) {
        } else {
          val act = () => {
            try {
              currentPuzzle = Some(parsePuzzleLine(text.trim))
              currentPuzzleText = text.trim
            } catch {
              case ContestException(msg, data) =>
                val text = if (data.isEmpty) msg else s"$msg, ${data.get.toString}"
                val errorText = s"Failed: $text"
                setText(errorText)
            }
          }
          dom.window.setTimeout(act, 50)
        }
      }
      puzzleReader.readAsText(puzzleFileInput.files(0))
    }
  }

  private val puzzleSolutionHandler: Function1[Event, Unit] = event => {
    clearPuzzleSolution()
    if (!puzzleSolutionFileInput.files(0).isInstanceOf[Blob]) {
      setText(NO_TASK_FILE)
    } else {
      val solutionReader = new FileReader()
      solutionReader.onloadend = _ => {
        val text = solutionReader.result.toString
        if (text == currentPuzzleSolutionText) {}
        else {
          val act = () => {
            try {
              currentTask = Some(parseTask(text))
              currentTaskText = text
            } catch {
              case ContestException(msg, data) =>
                val text = if (data.isEmpty) msg else s"$msg, ${data.get.toString}"
                val errorText = s"Failed: $text"
                setText(errorText)
            }
          }
          dom.window.setTimeout(act, 50)
        }
      }
      solutionReader.readAsText(puzzleSolutionFileInput.files(0))
    }
  }

  private val execHandler: Function1[Event, Unit] = event => {
    if (currentTask.isDefined && currentPuzzle.isDefined) {
      setText(PREPROCESSING_PUZZLE_TEXT)

      val act = () => {
        val task = currentTask.get
        val puzzle = currentPuzzle.get
        val text = PuzzleCheckingUtils.checkTaskForSpec(task, puzzle) match {
          case Left(value) => "Success!"  
          case Right(msg) => s"Failed: $msg"
        }
        setText(text)
      }

      // Oh, this is nasty...
      dom.window.setTimeout(act, 50)
    } else {
      setText(CANNOT_PROCESS_TEXT)
    }
  }


  def setText(text: String): Boolean = {
    textArea.textContent = text
    true
  }


}
