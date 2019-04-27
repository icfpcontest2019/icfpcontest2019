package lambda.geometry.integer.examples

import lambda.geometry.integer.{IPolygon, _}

object IPolygonExamples {

  val trianglePoly: IPolygon = mkIPoly((0, 0), (2, 0), (2, 2))
  val square: IPolygon = mkIPoly((1, 0), (0, 1), (-1, 0), (0, -1))
  val convexPoly: IPolygon = mkIPoly((0, 0), (2, 0), (2, 2), (0, 2))
  val convexPoly2: IPolygon = mkIPoly((1, -1), (2, 2), (0, 2), (0, 0))
  val nonConvexPoly5: IPolygon = mkIPoly((0, 0), (0, 2), (2, 2), (-1, 3))
  val bunnyEars : IPolygon = mkIPoly((0, 0), (4, 0), (3, 2), (2, 1), (1, 2))
  val convexPoly4: IPolygon = mkIPoly((1, 1), (1, 2), (0, 3), (0, 0))

  // More interesting cases
  val lShapedPolygon: IPolygon = mkIPoly((0, 0), (2, 0), (2, 1), (1, 1), (1, 3), (0, 3))
  val kittyPolygon: IPolygon = mkIPoly((0, 0), (5, 0), (5, 2), (4, 1), (1, 1), (0, 2))
  val simpleStarPolygon: IPolygon = mkIPoly((5, 0), (1, 1), (0, 5), (-1, 1), (-5, 0), (-1, -1), (0, -5), (1, -1))

  val weirdRectPolygon: IPolygon = mkIPoly((0, 0), (2, 0), (2, 1), (1, 1), (1, 2), (3, 2), (3, 3), (0, 3))
  val crazy : IPolygon = mkIPoly((3, -25), (-3, -25), (-3, -26), (0, -26), (0, -48), (1, -48), (1, -47), (120, -47), (120, 55), (1, 55), (1, -26), (3, -26))

  val simple3Rectangle: IPolygon = mkIPoly((0, 0), (4, 0), (4, 4), (0, 4))
  val flat3Polygon: IPolygon = mkIPoly((0, 0), (3, 0), (3, 1), (0, 1))

  val keyBug: IPolygon = mkIPoly((0, 0), (3, 0), (3, 3), (0, 3), (0, 2), (-4, 2), (-4, -2), (-3, -2), (-3, 1), (0, 1))

}

object WatchmanExamples {
  val room1 = mkIPoly((0, 0), (6, 0), (6, 1), (8, 1), (8, 2), (6, 2), (6, 3), (0, 3))
  val room2 = mkIPoly((0, 0), (2, 0), (2, -3), (3, -3),
    (3, -5), (4, -5), (4, -6), (1, -6), (1, -2), (-2, -2), (-2, -6), (-1, -6), (-1, -7), (-3, -7), (-3, -1), (-4, -1), (-4, 19), (-16, 19), (-16, -1), (-14, -1), (-14, -3), (-24, -3), (-24, 7), (-30, 7), (-30, -3), (-28, -3), (-28, -24), (-14, -24), (-14, -13), (-10, -13), (-10, -1), (-9, -1), (-9, -7), (-6, -7), (-6, -10), (-1, -10), (-1, -9), (4, -9), (4, -8), (6, -8), (6, -5), (6, -3), (5, -3), (5, 0), (4, 0), (4, 1), (6, 1), (6, -2), (9, -2), (9, -1), (12, -1), (12, 2), (9, 2), (9, 1), (8, 1), (8, 7), (6, 7), (6, 10), (3, 10), (3, 7), (4, 7), (4, 4), (0, 4))
  val room3 = mkIPoly((0, 0), (1, 0), (1, -6), (0, -6), (0, -3), (-3, -3), (-3, -4), (-5, -4), (-5, -7), (-3, -7), (-3, -6), (-2, -6), (-2, -9), (-1, -9), (-1, -15), (1, -15), (1, -9), (1, -8), (3, -8), (3, -23), (12, -23), (12, -8), (7, -8), (7, 0), (4, 0), (4, 2), (9, 2), (9, 4), (4, 4), (3, 4), (3, 10), (1, 10), (1, 9), (-4, 9), (-4, 7), (1, 7), (1, 4), (0, 4), (0, 2), (-2, 2), (-2, 1), (-3, 1), (-3, 6), (-6, 6), (-6, 4), (-7, 4), (-7, 14), (-13, 14), (-13, 4), (-12, 4), (-12, 1), (-6, 1), (-6, 0), (-15, 0), (-15, -1), (-6, -1), (-6, -2), (-2, -2), (-2, -1), (0, -1))
  val room4 = mkIPoly((0, 0), (1, 0), (1, -1), (-11, -1), (-11, -7), (1, -7), (1, -6), (2, -6), (2, -9), (5, -9), (5, -6), (4, -6), (4, 0), (4, 4), (3, 4), (3, 10), (4, 10), (4, 8), (5, 8), (5, 5), (6, 5), (6, 0), (9, 0), (9, 3), (15, 3), (15, 6), (9, 6), (9, 5), (8, 5), (8, 8), (7, 8), (7, 10), (6, 10), (6, 11), (11, 11), (11, 14), (6, 14), (6, 13), (5, 13), (5, 17), (3, 17), (3, 13), (3, 12), (2, 12), (2, 19), (5, 19), (5, 22), (2, 22), (2, 24), (22, 24), (22, 54), (2, 54), (2, 36), (0, 36), (0, 12), (-1, 12), (-1, 17), (-4, 17), (-4, 12), (-3, 12), (-3, 4), (0, 4))
  val room5 = mkIPoly((0, 0), (2, 0), (2, -1), (-3, -1), (-3, -4), (2, -4), (2, -3), (3, -3), (3, -7), (-1, -7), (-1, -8), (-4, -8), (-4, -11), (-1, -11), (-1, -10), (3, -10), (3, -9), (6, -9), (6, -3), (5, -3), (5, 0), (4, 0), (4, 1), (10, 1), (10, -7), (7, -7), (7, -10), (10, -10), (10, -9), (16, -9), (16, 1), (18, 1), (18, -3), (21, -3), (21, 1), (20, 1), (20, 2), (11, 2), (11, 6), (14, 6), (14, 9), (11, 9), (11, 8), (5, 8), (5, 2), (4, 2), (4, 4), (3, 4), (3, 12), (7, 12), (7, 14), (3, 14), (3, 16), (-1, 16), (-1, 22), (-2, 22), (-2, 28), (-5, 28), (-5, 22), (-4, 22), (-4, 16), (-3, 16), (-3, 4), (0, 4))
  val room6 = mkIPoly((0, 0), (1, 0), (1, -9), (2, -9), (2, -8), (6, -8), (6, -10), (7, -10), (7, -13), (5, -13), (5, -10), (2, -10), (2, -13), (1, -13), (1, -14), (-10, -14), (-10, -16), (1, -16), (7, -16), (7, -15), (10, -15), (10, -12), (11, -12), (11, -14), (14, -14), (14, -12), (13, -12), (13, -9), (10, -9), (10, -10), (9, -10), (9, -8), (8, -8), (8, -7), (13, -7), (13, -4), (8, -4), (8, -5), (2, -5), (2, 0), (4, 0), (4, 1), (6, 1), (6, -1), (9, -1), (9, 1), (8, 1), (8, 4), (11, 4), (11, 7), (8, 7), (4, 7), (4, 4), (2, 4), (2, 6), (-1, 6), (-1, 4), (0, 4), (0, 3), (-6, 3), (-6, -3), (0, -3))
  val room7 = mkIPoly((0, 0), (2, 0), (2, -4), (5, -4), (5, 0), (4, 0), (4, 1), (7, 1), (7, -3), (11, -3), (11, -11), (15, -11), (15, -3), (13, -3), (13, -2), (23, -2), (23, 4), (13, 4), (13, 1), (10, 1), (10, 2), (12, 2), (12, 5), (10, 5), (10, 4), (4, 4), (2, 4), (2, 5), (4, 5), (4, 6), (9, 6), (9, 7), (15, 7), (15, 10), (12, 10), (12, 18), (6, 18), (6, 13), (4, 13), (4, 10), (6, 10), (9, 10), (9, 9), (4, 9), (4, 8), (2, 8), (2, 7), (1, 7), (1, 11), (0, 11), (0, 13), (-3, 13), (-3, 11), (-2, 11), (-2, 7), (-1, 7), (-1, 4), (0, 4), (0, 3), (-6, 3), (-6, 2), (0, 2))
  val room8 = mkIPoly((0, 0), (1, 0), (1, -2), (-5, -2), (-5, -3), (-8, -3), (-8, 7), (-14, 7), (-14, -3), (-11, -3), (-11, -6), (-5, -6), (-5, -5), (1, -5), (1, -4), (2, -4), (2, -9), (5, -9), (5, -4), (4, -4), (4, 0), (4, 4), (2, 4), (2, 8), (1, 8), (1, 9), (4, 9), (4, 6), (5, 6), (5, 9), (7, 9), (7, 10), (14, 10), (14, 5), (16, 5), (16, 10), (17, 10), (17, 11), (7, 11), (7, 12), (5, 12), (5, 24), (-1, 24), (-1, 12), (1, 12), (1, 11), (-1, 11), (-1, 8), (-1, 4), (0, 4), (0, 2), (-2, 2), (-2, 8), (-3, 8), (-3, 10), (-6, 10), (-6, 8), (-5, 8), (-5, 2), (-4, 2), (-4, -1), (0, -1))
  val room9 = mkIPoly((0, 0), (1, 0), (1, -2), (-3, -2), (-3, -6), (-6, -6), (-6, -9), (-3, -9), (-3, -8), (1, -8), (1, -5), (2, -5), (2, -9), (-2, -9), (-2, -12), (2, -12), (2, -11), (5, -11), (5, -7), (9, -7), (9, -4), (5, -4), (5, -5), (4, -5), (4, 0), (4, 2), (13, 2), (13, 4), (4, 4), (0, 4), (0, 2), (-1, 2), (-1, 5), (2, 5), (2, 6), (8, 6), (8, 9), (2, 9), (2, 8), (-1, 8), (-1, 7), (-2, 7), (-2, 12), (-3, 12), (-3, 14), (-6, 14), (-6, 12), (-5, 12), (-5, 7), (-4, 7), (-4, 6), (-7, 6), (-7, 5), (-9, 5), (-9, 2), (-7, 2), (-7, 3), (-4, 3), (-4, 2), (-3, 2), (-3, -1), (0, -1))
  val room10 = mkIPoly((0, 0), (1, 0), (1, -36), (13, -36), (13, 0), (4, 0), (4, 4), (2, 4), (2, 5), (4, 5), (4, 6), (6, 6), (6, 7), (7, 7), (7, 1), (10, 1), (10, 4), (18, 4), (18, 8), (21, 8), (21, 0), (25, 0), (25, 8), (24, 8), (24, 11), (18, 11), (18, 10), (10, 10), (10, 7), (9, 7), (9, 10), (6, 10), (6, 9), (4, 9), (4, 8), (2, 8), (2, 7), (-1, 7), (-1, 6), (-2, 6), (-2, 8), (-3, 8), (-3, 10), (-4, 10), (-4, 13), (2, 13), (2, 19), (-4, 19), (-4, 16), (-7, 16), (-7, 10), (-6, 10), (-6, 8), (-5, 8), (-5, 6), (-5, 3), (-1, 3), (-1, 4), (0, 4), (0, 2), (-5, 2), (-5, -1), (0, -1))
}
