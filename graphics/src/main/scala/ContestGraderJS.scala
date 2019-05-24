
import lambda.geometry.integer.{IPoint, IPolygon}
import org.scalajs.dom
import org.scalajs.dom._
import org.scalajs.dom.raw._

import scala.scalajs.js
import scala.scalajs.js.Date
import scala.scalajs.js.annotation.JSExportTopLevel

/**
  * This file allows for client-side grading of contest applications 
  * using Scala.JS compilation
  *
  * @author Ilya Sergey
  */

object ContestGraderJS {

  type ButtonHandler = js.Function1[MouseEvent, _]

  val outTextField = "output"
  val submitButton = "submit"
  
  val DEFAULT_BUTTON_TEXT = "Booya!"
  val SUBMIT_TEXT = "Check"

  // Use this for indicating the output
  def mkTextField(targetNode: dom.Node, label: String,
                  initText: String = "Default text"): Unit = {
    val textField = document.createElement("text").asInstanceOf[HTMLTextAreaElement]
    textField.textContent = getCurrentTime()
    textField.id = label
    val parNode = document.createElement("p")
    parNode.appendChild(textField)
    targetNode.appendChild(parNode)
  }


  def mkButton(targetNode: dom.Node, label: String,
               initText: String = DEFAULT_BUTTON_TEXT,
               onClick: ButtonHandler): Unit = {
    val button = document.createElement("button").asInstanceOf[HTMLButtonElement]
    button.textContent = initText
    button.onclick = onClick
    val parNode = document.createElement("p")
    parNode.appendChild(button)
    targetNode.appendChild(parNode)
  }

  // Create input element
/*
  def mkFileInput(targetNode: dom.Node, text: String): Unit = {
    val parNode = document.createElement("p")
    val inputNode = document.createElement("input").asInstanceOf[HTMLInputElement]
    inputNode.setAttribute("type", "file")
    inputNode.setAttribute("id", "myInput")

    //val button = <button onclick={ event: Event => {println(text)} }>Click me!</button>
    val button = document.createElement("button").asInstanceOf[HTMLButtonElement]
    parNode.appendChild(inputNode)
    targetNode.appendChild(parNode)
  }
*/

  @JSExportTopLevel("graderNoGraphics")
  def main(canvas: html.Canvas): Unit = {
    val centered = document.createElement("center")
    document.body.appendChild(centered)


    //mkFileInput(centered, "Hmm")
    val checkHandler = (e: Event) => {
      val textArea = document.getElementById(outTextField).asInstanceOf[HTMLTextAreaElement]
      textArea.textContent = getCurrentTime()
    }
    
    val poly = IPolygon(List(IPoint(0,0), IPoint(1,0), IPoint(1,1), IPoint(0,1))) 

    mkTextField(centered, outTextField)
    mkButton(centered, submitButton, SUBMIT_TEXT, checkHandler)


    // val fileInput = document.getElementById("myInput").asInstanceOf[HTMLInputElement]

//    fileInput.onchange = event => {
//      val reader = new FileReader()
//      reader.onload = event => {
//        println(reader.result)
//      }
//      reader.readAsText(fileInput.files(0))
//    }
  }

  private def getCurrentTime() = {
    val date = new Date()
    s"${date.toDateString()} ${date.toTimeString()}"
  }
}
