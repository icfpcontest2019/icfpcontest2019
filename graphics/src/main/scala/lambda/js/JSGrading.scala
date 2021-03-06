package lambda.js

import lambda.contest.ContestConstants.Action
import lambda.contest.ContestErrorMessages.{BAD_BOOSTER_FORMAT, BAD_SOLUTION_FORMAT, MALFORMED_TASK}
import lambda.contest.checkers.{TaskExecution, TaskMatrix}
import lambda.contest.parsers.{BoosterBuyingParser, ContestSolutionParser, ContestTaskParser}
import lambda.contest.{Booster, ContestConstants, ContestException, ContestTask}
import org.scalajs.dom
import org.scalajs.dom.document
import org.scalajs.dom.raw._

import scala.scalajs.js
import scala.scalajs.js.Date

/**
  * @author Ilya Sergey
  */
trait JSGrading {

  type ProcessedTask = (TaskMatrix, Int, Int)
  type TaskSolution = List[List[Action]]

  val canvasId = "canvas"
  val mainSectionID = "main_section"
  val outTextFieldId = "output"
  val execButtonId = "execute_solution"
  val submitTaskId = "submit_task"
  val submitSolutionId = "submit_solution"
  val submitBoostersId = "submit_boosters"
  val speedTextId = "speed_text"

  type ButtonHandler = js.Function1[MouseEvent, _]
  
  val DEFAULT_BUTTON_TEXT = "Booya!"
  val SUBMIT_TASK_TEXT = "Task file"
  val SUBMIT_SOLUTION_TEXT = "Solution file"
  val SUBMIT_BOOSTERS_TEXT = "Additional boosters (optional)"
  val CHECK_TEXT = "Check"
  val CHECK_AND_RENDER_TEXT = "Upload files"
  val EXECUTE_TEXT = "Prepare to Run"

  val SUBMIT_PUZZLE_TEXT = "Puzzle file"

  val CANNOT_PROCESS_TEXT = "Cannot check: some parts of the input are missing or malformed"
  val PREPROCESSING_TEXT = "Pre-processing and validating the task..."
  val PREPROCESSING_PUZZLE_TEXT = "Validating the puzzle solution..."
  val SPACE_TO_RUN_TEXT = "Press SPACE (s) to begin execution"
  val SPACE_TO_RESUME_TEXT = "Press SPACE (s) to resume execution"
  val RUNNING_TEXT = "Running"
  val CHECKING_TEXT = "Checking the solution..."
  val FAILED_TEXT = "Failed to cover the full task"
  val LOADING_TEXT = s"Processing your solution..."
  val UPLOAD_FILES = "Upload the task and solution files"
  val NO_TASK_FILE = "No task file provided"
  val NO_PUZZLE_FILE = "No puzzle file provided"
  val NO_SOLUTION_FILE = "No solution file provided"
  val UPLOADING_TASK = "Uploading task description..."
  val UPLOADED_TASK = "Done uploading task description"
  val UPLOADED_PUZZLE = "Done uploading puzzle description"
  val UPLOADING_SOLUTION = "Uploading solution file..."
  val UPLOADED_SOLUTION = "Done uploading solution"

  lazy val taskFileInput = document.getElementById(submitTaskId).asInstanceOf[HTMLInputElement]
  lazy val solutionFileInput = document.getElementById(submitSolutionId).asInstanceOf[HTMLInputElement]
  lazy val boostersFileInput = document.getElementById(submitBoostersId).asInstanceOf[HTMLInputElement]
  lazy val execButton = document.getElementById(execButtonId).asInstanceOf[HTMLButtonElement]
  lazy val textArea = document.getElementById(outTextFieldId)

  var currentTaskText: String = ""
  var currentSolutionText: String = ""
  var currentBoosterText: String = ""
  var currentTask: Option[ContestTask] = None
  var currentBoosters: List[Booster.Value] = Nil
  var currentSolution: Option[TaskSolution] = None
  var currentState: Option[TaskExecution] = None
  var currentMatrix: Option[ProcessedTask] = None

  /* ---------------------------------------------------------------- */
  /*                          Parsing submissions                     */
  /* ---------------------------------------------------------------- */

  def parseTask(taskText: String): ContestTask = {
    val res = ContestTaskParser(taskText)
    if (res.successful) return res.get
    throw ContestException(MALFORMED_TASK)
  }
  
  def parseSolution(solText: String): List[List[ContestConstants.Action]] = {
    val res = ContestSolutionParser(solText)
    if (res.successful) return res.get
    throw ContestException(BAD_SOLUTION_FORMAT)
  }

  def parseBoosters(text: String): List[Booster.Value] = {
    val res = BoosterBuyingParser(text)
    if (res.successful) return res.get
    throw ContestException(BAD_BOOSTER_FORMAT)
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
