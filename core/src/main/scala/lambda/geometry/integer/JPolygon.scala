package lambda.geometry.integer

/**
  * Fast implementation adapted from Java AWT
  * 
  * @author Ilya Sergey
  */
case class JPolygon(xpoints: Array[Int], ypoints: Array[Int], npoints: Int,
                    boundingBox: JRectangle) {

  def contains(x: Double, y: Double): Boolean = {
    if (npoints <= 2 || !boundingBox.contains(x, y)) return false
    var hits = 0
    var lastx = xpoints(npoints - 1)
    var lasty = ypoints(npoints - 1)
    var curx = 0
    var cury = 0

    // Walk the edges of the polygon
    var i = 0

    def tick(): Unit = {
      lastx = curx
      lasty = cury
      i = i + 1
    }

    while (i < npoints) {
        curx = xpoints(i)
        cury = ypoints(i)
        if (cury == lasty) {
          tick()
        } else {
          var leftx = 0
          
          def tail1(): Unit = {
            var test1 = .0
            var test2 = .0
            if (cury < lasty) {
              if (y < cury || y >= lasty) {} 
              else if (x < leftx) {
                hits += 1
              } else {
                test1 = x - curx
                test2 = y - cury
                if (test1 < (test2 / (lasty - cury) * (lastx - curx))) {
                  hits += 1
                }
              }
              tick()
            } else {
              if (y < lasty || y >= cury) {} 
              else if (x < leftx) {
                hits += 1
              } else {
                test1 = x - lastx
                test2 = y - lasty
                if (test1 < (test2 / (lasty - cury) * (lastx - curx))) {
                  hits += 1
                }
              }
              tick()
            }
          }
          
          // Don't touch stuff below
          if (curx < lastx) {
            if (x >= lastx) {
              tick()
            } else {
              leftx = curx
              tail1()
            }
          } else {
            if (x >= curx) {
              tick()
            } else {
              leftx = lastx
              tail1()
            }
          }
          
        }
    }

    (hits & 1) != 0
  }

}

case class JRectangle(x: Int, y: Int, width: Int, height: Int) {

  def contains(x: Double, y: Double) = {
    val x0 = this.x
    val y0 = this.y
    x >= x0 && y >= y0 && x < x0 + width && y < y0 + height
  }

}
