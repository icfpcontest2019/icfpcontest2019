package example

import lambda.geometry.floating.FPoint
import org.scalajs.dom
import org.scalajs.dom.html

import scala.scalajs.js.annotation.JSExportTopLevel

object SpaceInvaders {
  private lazy val canvas =
    dom.document
      .getElementById("canvas")
      .asInstanceOf[html.Canvas]

  private lazy val ctx =
    canvas.getContext("2d")
      .asInstanceOf[dom.CanvasRenderingContext2D]

  private val iHeight = dom.window.innerHeight.toInt
  private val iWidth = dom.window.innerWidth.toInt
  canvas.height = iHeight
  canvas.width = iWidth

  private var count = 0
  private var player = FPoint(iHeight / 2, iWidth / 2)
  private val corners = Seq(FPoint(255, 255), FPoint(0, 255), FPoint(128, 0))

  private var bullets = Seq.empty[FPoint]
  private var enemies = Seq.empty[FPoint]

  private var wave = 1

  private def run = {
    count += 1
    bullets = bullets.map(
      p => FPoint(p.x, p.y - 5)
    )

    if (enemies.isEmpty){
      enemies = for{
        x <- 0 until canvas.width by 50
        y <- 0 until wave
      } yield {
        FPoint(x, 50 + y * 50)
      }
      wave += 1
    }

    enemies = enemies.filter( e =>
      !bullets.exists(b =>
        (e - b).length < 5
      )
    )
    enemies = enemies.map{ e =>
      val i = count % 200
      if (i < 50) e.copy(x = e.x - 0.2)
      else if (i < 100) e.copy(y = e.y + 0.2)
      else if (i < 150) e.copy(x = e.x + 0.2)
      else e.copy(y = e.y + 0.2)
    }


    if (keysDown(38)) player += FPoint(0, -2)
    if (keysDown(37)) player += FPoint(-2, 0)
    if (keysDown(39)) player += FPoint(2, 0)
    if (keysDown(40)) player += FPoint(0, 2)
  }

  private def draw = {
    ctx.fillStyle = "black"
    ctx.fillRect(0, 0, canvas.width, canvas.height)

    ctx.fillStyle = "white"
    ctx.fillRect(player.x - 5, player.y - 5, 10, 10)

    ctx.fillStyle = "yellow"
    for (enemy <- enemies){
      ctx.fillRect(enemy.x - 5, enemy.y - 5, 10, 10)
    }
    ctx.fillStyle = "red"
    for (bullet <- bullets){
      ctx.fillRect(bullet.x - 2, bullet.y - 2, 4, 4)
    }
  }

  private val keysDown = collection.mutable.Set.empty[Int]
  @JSExportTopLevel("spaceInvaders")
  def main(): Unit = {

    dom.console.log("main")
    dom.window.onkeypress = {e: dom.KeyboardEvent =>
      if (e.keyCode == 32) bullets = player +: bullets
    }
    dom.window.onkeydown = {e: dom.KeyboardEvent =>
      keysDown.add(e.keyCode)
    }
    dom.window.onkeyup = {e: dom.KeyboardEvent =>
      keysDown.remove(e.keyCode.toInt)
    }
    dom.window.setInterval(() => {run; draw}, 20)
  }
}

