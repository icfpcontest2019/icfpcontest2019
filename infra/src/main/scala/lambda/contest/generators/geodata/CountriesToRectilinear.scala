package lambda.contest.generators.geodata

import java.awt.{BorderLayout, Color, Graphics}
import java.io.File

import javax.swing.{BoxLayout, JFrame, JPanel}
import lambda.contest.generators.{GeneratorFileUtil, PolygonToRender}
import lambda.geometry.floating.{FPoint, FSegment, RenderUtils}
import lambda.geometry.integer.{IPoint, IPolygon, IntersectionUtils}
import lambda.util.FileUtil

import scala.collection.mutable

/**
  * @author Ilya Sergey
  */
object CountriesToRectilinear {

  val countriesInputPath = "./infra/src/main/resources/raw/geodata/countries/"
  val statesInputPath = "./infra/src/main/resources/raw/geodata/states/usa"
  val liftedOrigin = FPoint(-2, -2)
  val JSON = ".json"

  val countriesOutputPath = "./infra/src/main/resources/geoshapes/countries"
  val statesOutputPath = "./infra/src/main/resources/geoshapes/usa_states"


  def main(args: Array[String]): Unit = {
    if (args.length > 0 && args(0) == "all") {
      processShapes(countriesInputPath, countriesOutputPath, 100)
      processShapes(countriesInputPath, countriesOutputPath, 200)
      processShapes(countriesInputPath, countriesOutputPath, 400)
      processShapes(countriesInputPath, countriesOutputPath, 800)
      
      processShapes(statesInputPath, statesOutputPath, 100)
      processShapes(statesInputPath, statesOutputPath, 200)
      processShapes(statesInputPath, statesOutputPath, 400)
      processShapes(statesInputPath, statesOutputPath, 800)
    } else {
      val path = s"$countriesInputPath/${args(0)}$JSON"
      val poly = pixelisedShape(path, args(1).toInt)
      renderCountry(poly)
    }
  }
  
  
  def processShapes(inputPath: String, outPath: String, boxSize: Int): Unit = {
    val countryDir = new File(inputPath)
    assert(countryDir.exists())
    for {
      f <- countryDir.listFiles().toList.sortBy(_.getName)
      if f.getName.endsWith(JSON)
      cname = f.getName.stripSuffix(JSON)
      cpath = f.getAbsolutePath
    } {
      try {
        print(s"${cname.capitalize}, size $boxSize ... ")
        val poly = pixelisedShape(cpath, boxSize)
        val dir = s"$outPath/$boxSize"
        val dirFile = new File(dir) 
        if (!dirFile.exists()) {
          dirFile.mkdirs()
        } 
        val out = s"$dir/$cname${GeneratorFileUtil.noObstacleExtension}"
        FileUtil.writeToNewFile(out, poly.vertices.mkString(","))
        println(s"done! Size: ${poly.vertices.size}, box: ${poly.boundingBox}")
      } catch {
        case _: Throwable => println("failed. :(")
      }
    }
    
    println()
    println()
    
  }

  ///////////////////////////////////////////////////////////////////////////////////
  //                      Obtaining the rectilinear polygon
  ///////////////////////////////////////////////////////////////////////////////////


  private def pixelisedShape(path: String, boxSize: Int): IPolygon = {
    val realBoxSize = (boxSize - 1) / 2
    val (matrix, dx, dy) = makeShapeArray(path, realBoxSize)
    val (x0, y0) = getLowestStartingPoint(matrix, dx, dy)
    val polygon = walkArrayInDirection(matrix, x0, y0)
    assert(polygon.isRectilinear)
    assert(polygon.isWellFormed)
    val pixelCountry = polygon.shiftToOrigin
    pixelCountry
  }

  object WalkDir extends Enumeration {
    type WalkDir = Value
    val Right, Up, Left, Down = Value
  }

  import WalkDir._

  def walkArrayInDirection(matrix: Array[Array[Int]], x0: Int, y0: Int): IPolygon = {
    import WalkDir._
    val q = new mutable.Queue[IPoint]()

    var x = x0
    var y = y0
    q.enqueue(IPoint(x, y))

    var dir = Right

    do {
      val (xNew, yNew, dirNew) = getNewPointAndDirection(matrix, x, y, dir)
      assert(math.abs(xNew - x) + math.abs(yNew - y) == 1)
      if (dirNew != dir && !(xNew == x0 && yNew == y0)) {
        q.enqueue(IPoint(xNew, yNew))
      }
      dir = dirNew
      x = xNew
      y = yNew
    } while (!(x == x0 && y == y0))

    IPolygon(q.toList)
  }

  private def getNewPointAndDirection(matrix: Array[Array[Int]], x: Int, y: Int,
                                      dir: WalkDir.Value): (Int, Int, WalkDir.Value) = dir match {
    case Right =>
      val (a, b) = (matrix(x)(y), matrix(x)(y - 1))
      assert(a == 1)
      assert(b == 0)
      val newDir = (matrix(x + 1)(y), matrix(x + 1)(y - 1)) match {
        case (0, 0) => Up
        case (_, 1) => Down
        case (1, 0) => Right
        case z => assert(false, s"This can't happen: x = $x, y = $y, (a, b) = $z"); Right
      }
      (x + 1, y, newDir)

    case Up =>
      val (a, b) = (matrix(x - 1)(y), matrix(x)(y))
      assert(a == 1)
      assert(b == 0)
      val newDir = (matrix(x - 1)(y + 1), matrix(x)(y + 1)) match {
        case (0, 0) => Left
        case (_, 1) => Right
        case (1, 0) => Up
        case z => assert(false, s"This can't happen: x = $x, y = $y, (a, b) = $z"); Up
      }

      (x, y + 1, newDir)

    case Left =>
      val (a, b) = (matrix(x - 1)(y - 1), matrix(x - 1)(y))
      assert(a == 1)
      assert(b == 0)
      val newDir = (matrix(x - 2)(y - 1), matrix(x - 2)(y)) match {
        case (0, 0) => Down
        case (_, 1) => Up
        case (1, 0) => Left
        case z => assert(false, s"This can't happen: x = $x, y = $y, (a, b) = $z"); Left
      }
      (x - 1, y, newDir)

    case Down =>
      val (a, b) = (matrix(x)(y - 1), matrix(x - 1)(y - 1))
      assert(a == 1)
      assert(b == 0)
      val newDir = (matrix(x)(y - 2), matrix(x - 1)(y - 2)) match {
        case (0, 0) => Right
        case (_, 1) => Left
        case (1, 0) => Down
        case z => assert(false, s"This can't happen: x = $x, y = $y, (a, b) = $z"); Down
      }
      (x, y - 1, newDir)
  }


  ///////////////////////////////////////////////////////////////////////////////////
  //                              Rendering
  ///////////////////////////////////////////////////////////////////////////////////


  private def renderCountry(country: IPolygon) = {
    val pp = PolygonToRender(country.toFPolygon)

    val frame = new JFrame()
    val polygonPanel = new JPanel() {
      override def paint(g: Graphics) = {
        pp.fillWhiteBackground(g)
        pp.drawPoly(g, pp.polygon, Color.BLACK)
      }
    }

    frame.setLayout(new BoxLayout(frame.getContentPane, BoxLayout.Y_AXIS))
    frame.add(polygonPanel, BorderLayout.NORTH)
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    val size = pp.frameSize
    frame.setSize(size._1, size._2)
    frame.setVisible(true)
  }


  ///////////////////////////////////////////////////////////////////////////////////
  //                        Creating the boundary array
  ///////////////////////////////////////////////////////////////////////////////////


  def makeShapeArray(geoPath: String, boxSize: Int): (Array[Array[Int]], Int, Int) = {
    val country = GeoHelper.getCountryScaled(geoPath, boxSize)
    val (lowEnd, _) = RenderUtils.getPolygonBoundingBox(country)
    val polygon = country.shiftToOrigin(lowEnd).shiftToOrigin(liftedOrigin)
    val (_, FPoint(xr, yr)) = RenderUtils.getPolygonBoundingBox(polygon)

    val dx = (xr + 2.5).toInt
    val dy = (yr + 2.5).toInt
    val matrix = Array.fill(dx)(Array.fill(dy)(0))

    for {e@FSegment(a, b) <- polygon.edges
         IPoint(i, j) <- IntersectionUtils.cellsCrossedBySegment(e)} {
      matrix(i)(j) = 1
    }
    (matrix, dx, dy)
  }

  def getLowestStartingPoint(matrix: Array[Array[Int]], dx: Int, dy: Int): (Int, Int) = {
    for (j <- 0 until dy; i <- 0 until dx) {
      if (matrix(i)(j) > 0) return (i, j)
    }
    (-1, -1)
  }

  def printShapeArray(matrix: Array[Array[Int]], xsize: Int, ysize: Int, z: (Int, Int)) = {
    val (x0, y0) = z
    for (i <- 0 until xsize + 2) print("O")
    println()
    for (j1 <- 1 to ysize) {
      val j = ysize - j1
      print("O")
      for (i <- 0 until xsize) {
        if (i == x0 && j == y0) {
          print("#")
        } else {
          val b = matrix(i)(j)
          print(if (b > 0) "X" else " ")
        }
      }
      print("O")
      println()
    }
    for (i <- 0 until xsize + 2) print("O")
    println()
  }


}
