package lambda.js

import lambda.contest.ContestErrorMessages.{BAD_SOLUTION_FORMAT, MALFORMED_TASK}
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

  val canvasId = "canvas"
  val mainSectionID = "main_section"
  val outTextFieldId = "output"
  val checkButtonId = "check_solution"
  val submitTaskId = "submit_task"
  val submitSolutionId = "submit_solution"

  type ButtonHandler = js.Function1[MouseEvent, _]
  
  val DEFAULT_BUTTON_TEXT = "Booya!"
  val SUBMIT_TASK_TEXT = "Task file"
  val SUBMIT_SOLUTION_TEXT = "Solution file"
  val CHECK_TEXT = "Check"
  val CHECK_AND_RENDER_TEXT = "Upload files"
  
  val PREPROCESSING_TEXT = "Validating and pre-processing the task..."
  val CHECKING_TEXT = "Checking the solution..."
  val FAILED_TEXT = "Failed to cover the full task"
  val LOADING_TEXT = s"Processing your solution..."
  val UPLOAD_FILES = "Upload the task and solution files"
  val NO_TASK_FILE = "No task file provided"
  val NO_SOLUTION_FILE = "No solution file provided"
  val UPLOADED_TASK = "Uploaded task description"
  val UPLOADED_ALL = "Uploaded task and solution"


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
    if (!taskFileInput.files(0).isInstanceOf[Blob]) {
      setText(NO_TASK_FILE)
      return 
    }
    
    val taskReader = new FileReader()
    taskReader.onloadend = event => {
      val text = taskReader.result.toString
      processSolution(text)
    }
    taskReader.readAsText(taskFileInput.files(0))
  }

  def processSolution(taskText: String): Unit = {
    if (!solutionFileInput.files(0).isInstanceOf[Blob]) {
      setText(NO_SOLUTION_FILE)
      return
    }

    val solutionReader = new FileReader()
    solutionReader.onloadend = event => {
      val solText = solutionReader.result.toString
      runSolution(taskText, solText)
    }
    solutionReader.readAsText(solutionFileInput.files(0))
  }

  def runSolution(taskText: String, solutionText: String): Unit

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

  def mkButton(targetNode: dom.Node, 
               id: String,
               initText: String = DEFAULT_BUTTON_TEXT): Unit = {
    val button = document.createElement("button").asInstanceOf[HTMLButtonElement]
    button.textContent = initText
    button.id = id
    val parNode = document.createElement("p")
    parNode.appendChild(button)
    targetNode.appendChild(parNode)
  }

  def mkCanvas(targetNode: dom.Node, id: String): Unit = {
    val canvas = document.createElement("canvas").asInstanceOf[HTMLButtonElement]
    val parNode = document.createElement("p")
    parNode.appendChild(canvas)
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
