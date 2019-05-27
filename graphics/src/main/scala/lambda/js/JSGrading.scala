package lambda.js

import lambda.contest.ContestErrorMessages.{BAD_SOLUTION_FORMAT, MALFORMED_TASK}
import lambda.contest.checkers.TaskCreationUtils.contestTaskToMatrix
import lambda.contest.checkers.TaskExecution
import lambda.contest.parsers.{ContestSolutionParser, ContestTaskParser}
import lambda.contest.{ContestConstants, ContestException, ContestTask}
import org.scalajs.dom
import org.scalajs.dom.document
import org.scalajs.dom.raw._

import scala.scalajs.js
import scala.scalajs.js.Date

/**
  * @author Ilya Sergey
  */
trait JSGrading {

  type ButtonHandler = js.Function1[MouseEvent, _]
  
  val DEFAULT_BUTTON_TEXT = "Booya!"
  val SUBMIT_TASK_TEXT = "Task file"
  val SUBMIT_SOLUTION_TEXT = "Solution file"
  val SUBMIT_TEXT = "Check"
  val PREPROCESSING_TEXT = "Validating and pre-processing the task..."
  val CHECKING_TEXT = "Checking the solution..."
  val FAILED_TEXT = "Failed to cover the full task"
  val LOADING_TEXT = s"Processing your solution..."


  val taskFileInput : HTMLInputElement
  val solutionFileInput : HTMLInputElement
  def setText(s: String) : Unit

  /* ---------------------------------------------------------------- */
  /*                          Parsing submissions                     */
  /* ---------------------------------------------------------------- */

  def parseTask(taskText: String): ContestTask = {
    val res = ContestTaskParser(taskText)
    if (res.successful) return res.get
    throw ContestException(MALFORMED_TASK, "")
  }


  def parseSolution(solText: String): List[List[ContestConstants.Action]] = {
    val res = ContestSolutionParser(solText)
    if (res.successful) return res.get
    throw ContestException(s"$BAD_SOLUTION_FORMAT", "")
  }

  /* ---------------------------------------------------------------- */
  /*                          A number of callbacks                   */
  /* ---------------------------------------------------------------- */

  def processTaskAndSolution(): Unit = {
    val taskReader = new FileReader()
    taskReader.onloadend = event => {
      val text = taskReader.result.toString
      processSolution(text)
    }
    taskReader.readAsText(taskFileInput.files(0))
  }


  def processSolution(taskText: String): Unit = {
    val solutionReader = new FileReader()
    solutionReader.onloadend = event => {
      val solText = solutionReader.result.toString
      checkSolution(taskText, solText)
    }
    solutionReader.readAsText(solutionFileInput.files(0))
  }

  /* ---------------------------------------------------------------- */
  /*                          Solution checking                       */
  /* ---------------------------------------------------------------- */

  def checkSolution(taskText: String, solutionText: String): Unit = {
    try {
      val task@ContestTask(_, init, _, _) = parseTask(taskText)
      val moves = parseSolution(solutionText)
      setText(PREPROCESSING_TEXT)
      val t0 = new Date().getTime()
      val (matrix, dx, dy) = contestTaskToMatrix(task)
      val t1 = new Date().getTime()
      val state = TaskExecution.createState(matrix, dx, dy, init, moves, Nil)
      setText(CHECKING_TEXT)
      val finalState = state.evalSolution()
      val resultText = finalState match {
        case Some(steps) =>
          val t2 = new Date().getTime()
          val tPre = (t1 - t0) / 1000
          val tMain = (t2 - t1) / 1000
          val s1 = "Success! \n"
          val s2 = s"Your solution took $steps time units. \n"
          val s3 = s"Pre-processing: $tPre sec, checking: $tMain sec."
          s1 + s2 + s3
        case None => FAILED_TEXT
      }
      setText(resultText)
    } catch {
      case ContestException(msg, data) =>
        val text = if (data.toString.isEmpty) msg else s"$msg (${data.toString})"
        val errorText = s"Failed: $text"
        setText(errorText)
    }
  }

  /* ---------------------------------------------------------------- */
  /*                          Various DOM elements                    */
  /* ---------------------------------------------------------------- */

  // Use this for indicating the output
  def mkTextField(targetNode: dom.Node, id: String,
                  text: String = ""): Unit = {
    val textField = document.createElement("text").asInstanceOf[HTMLTextAreaElement]
    textField.textContent = text
    textField.id = id
    val parNode = document.createElement("p")
    parNode.appendChild(textField)
    targetNode.appendChild(parNode)
  }

  def mkButton(targetNode: dom.Node, id: String,
               initText: String = DEFAULT_BUTTON_TEXT,
               onClick: ButtonHandler): Unit = {
    val button = document.createElement("button").asInstanceOf[HTMLButtonElement]
    button.textContent = initText
    button.onclick = onClick
    val parNode = document.createElement("p")
    parNode.appendChild(button)
    targetNode.appendChild(parNode)
  }

  def mkFileInput(targetNode: dom.Node, id: String, text: String = ""): Unit = {
    val parNode = document.createElement("p")
    val brNode = document.createElement("br")
    val label = document.createElement("label").asInstanceOf[HTMLLabelElement]
    label.htmlFor = id
    label.textContent = s"$text    "
    parNode.appendChild(label)

    val textInput = document.createElement("input").asInstanceOf[HTMLInputElement]
    textInput.id = id
    textInput.textContent = text
    textInput.`type` = "file"
    parNode.appendChild(brNode)
    parNode.appendChild(textInput)
    targetNode.appendChild(parNode)
  }


  protected def getCurrentTime() = {
    val date = new Date()
    s"${date.toDateString()} ${date.toTimeString()}"
  }

}
