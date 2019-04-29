package lambda.geometry.floating.examples

import lambda.geometry.floating.FPolygonUtils._
import lambda.geometry.floating.FPointUtils._
import lambda.geometry.floating._
import lambda.geometry.floating.triangles.Triangle


/**
  * @author Ilya Sergey
  */

object FPolygonExamples {

  // Some basic polygons

  // All polygons must have their interior on the left side of traversal!
  val trianglePoly: FPolygon = Seq((0, 0), (2, 0), (2, 2))
  val square: FPolygon = Seq((1, 0), (0, 1), (-1, 0), (0, -1))
  val convexPoly: FPolygon = Seq((0, 0), (2, 0), (2, 2), (0, 2))
  val convexPoly2: FPolygon = Seq((1, -1), (2, 2), (0, 2), (0, 0))
  val convexPoly3: FPolygon = Seq((0, 0), (2, 0), (2, 2), (0.4, 0.5))
  val simpleNonConvexPoly: FPolygon = Seq((0, 0), (2, 0), (2, 2), (1, 0.5))
  val nonConvexPoly5: FPolygon = Seq((0, 0), (0, 2), (2, 2), (-1, 3))
  val bunnyEars : FPolygon = Seq((0, 0), (4, 0), (3, 2), (2, 1), (1, 2))

  val convexPoly4: FPolygon = Seq((1, 1), (1, 2), (0, 3), (0, 0))

  // More interesting cases
  val lShapedPolygon: FPolygon = Seq((0, 0), (2, 0), (2, 1), (1, 1), (1, 3), (0, 3))
  val kittyPolygon: FPolygon = Seq((0, 0), (5, 0), (5, 2), (4, 1), (1, 1), (0, 2))
  val simpleStarPolygon: FPolygon = Seq((5, 0), (1, 1), (0, 5), (-1, 1), (-5, 0), (-1, -1), (0, -5), (1, -1))

  val weirdRectPolygon: FPolygon = Seq((0, 0), (2, 0), (2, 1), (1, 1), (1, 2), (3, 2), (3, 3), (0, 3))
  val shurikenPolygon: FPolygon = Seq((5, 0), (2, 0.5), (0, 5), (0.2, 3), (-2, -2), (-0.5, 0))
  val crazy : FPolygon = Seq((3, -25), (-3, -25), (-3, -26), (0, -26), (0, -48), (1, -48), (1, -47), (120, -47), (120, 55), (1, 55), (1, -26), (3, -26))

  val sand4: FPolygon = Seq((0, 0), (2, 0), (2, 1), (1.7, 1), (1.5, 0.4), (1.3, 1), (0, 1))
  val sand5: FPolygon = Seq((0.5, 4), (2, 5), (2.4, 4), (2.7, 5), (2.8, 5.5), (4, 6), (-3, 6), (-0.5, 4), (-0.5, 2))
  val sand3: FPolygon = Seq((2, 5), (2.4, 4), (2.7, 5), (2.8, 5.5), (4, 6), (2, 6))
  val lshaped3: FPolygon = Seq((0, 0), (0, 3), (1, 3), (1, 2.5), (1.5, 2), (1, 1), (2, 1), (2, 4), (-1, 4), (-1, -1), (2, -1), (2, 0))
  val tHorror: FPolygon = Seq((1, 3), (2, 1), (3, 3), (2, 3), (2, 4))

  val chvatal_comb: FPolygon = Seq((5, 2), (4.5, 1), (4, 1),
    (3.5, 2), (3, 1), (2.5, 1),
    (2, 2), (1.5, 1), (1, 1),
    (0.5, 2), (0, 0), (5, 0))

  val chvatal_comb1: FPolygon = Seq((5, 2), (4.5, 1), (4, 1),
    (3.5, 2), (3, 1), (2.5, 1),
    (2, 2), (1.5, 1), (1, 1),
    (0.5, 2), (0, 0.7), (5, 0.7))

  val simple3Rectangle: FPolygon = Seq((0, 0), (4, 0), (4, 4), (0, 4))
  val flat3Polygon: FPolygon = Seq((0, 0), (3, 0), (3, 1), (0, 1))

  val polarBug: FPolygon = Seq((0.0, 0.0), (3.0, 0.0), (3.0, 1.0), (5.0, 1.0), (5.0, 2.0), (3.0, 2.0), (3.0, 3.0), (0.0, 3.0))
  val piBug: FPolygon = Seq((0, 0), (3, 0), (3, 1), (4, 1), (4, -5), (5, -5), (5, 1), (11, 1), (11, 2), (3, 2), (3, 3), (0, 3))

  // [Fixed] Wrong results with (s = (3, 3))
  val keyBug: FPolygon = Seq((0, 0), (3, 0), (3, 3), (0, 3), (0, 2), (-4, 2), (-4, -2), (-3, -2), (-3, 1), (0, 1))
  // [Fixed ]Bug with s = (7, 3)
  val strangeKey = FPolygon(Seq((1, 2), (1, -3), (4, -3), (4, -1), (3, -1), (3, -2), (2, -2), (2, 1), (7, 1), (7, 3), (6, 3), (6, 2)))
  // Triangulation bug
  val triangBug = FPolygon(Seq((0, 0), (1, -3), (2, -3), (1, -2), (1, 0), (-1, 1), (-1, 2), (-2, 4), (-1, 0)))


  // Rays
  val ray1 = Ray2D(origin2D, quarter_pi)
  val ray2 = Ray2D(origin2D, half_pi)
  val ray3 = Ray2D((0, 1), half_pi)

  // Proper triangles
  val triangle1 = new Triangle((0, 0), (2, 0), (2, 2))
  val triangle2 = new Triangle((1, -1), (1.5, -1), (1, 2))
  val triangle3 = new Triangle((0, 0), (2, 0), (1, 6))
  val triangle4 = new Triangle((0, 0), (6, 0), (3, 1))

  val big_rect = FPolygon(Seq((0, 0), (1, 0), (1, -1), (-3, -1), (-3, -2), (-2, -2), (-2, -3), (-7, -3), (-7, -4), (-2, -4), (-2, -9), (-1, -9), (-1, -2), (1, -2), (1, -3), (2, -3), (2, -2), (3, -2), (3, -4), (4, -4), (4, -2), (5, -2
      ), (5, -3), (6, -3), (6, -2), (8, -2), (8, -1), (2, -1), (2, 0), (3, 0),
    (3, 1), (2, 1), (2, 2), (9, 2), (9, 3), (8, 3), (8, 4), (9, 4), (9, 5), (
        8, 5), (8, 6), (7, 6), (7, 3), (6, 3), (6, 6), (5, 6), (5, 3), (4, 3), (4
        , 9), (3, 9), (3, 8), (1, 8), (1, 7), (3, 7), (3, 3), (2, 3), (2, 6), (1,
        6), (1, 5), (0, 5), (0, 12), (-1, 12), (-1, 11), (-2, 11), (-2, 13), (-3
        , 13), (-3, 11), (-4, 11), (-4, 18), (-5, 18), (-5, 17), (-7, 17), (-7, 16),
    (-5, 16), (-5, 11), (-8, 11), (-8, 10), (-1, 10), (-1, 5), (-2, 5), (-2, 8),
    (-3, 8), (-3, 5), (-5, 5), (-5, 4), (-4, 4), (-4, 3), (-9, 3), (-9, 2), (-4, 2), (-4, 0), (-3, 0), (-3, 4), (1, 4), (1, 3), (-1, 3), (-1,
        2), (1, 2), (1, 1), (0, 1)))

  val chebur1 = FPolygon(Seq((-8.705819241042366, -0.6521412370374251), (-9.65687575733752, -0.3431242426624763), (-10.65687575733752, -0.3431242426624759), (-11.607932273632674, -0.6521412370374229), (-11.020147021340199, 0.1568757573375239), (-10.71113002696525, 1.1079322736326769), (-10.71113002696525, 2.1079322736326773), (-11.020147021340195, 3.058988789927832), (-11.607932273632667, 3.86800578430278), (-12.416949268007613, 4.455791036595254), (-11.416949268007613, 4.455791036595253), (-10.46589275171246, 4.764808030970199), (-9.656875757337511, 5.352593283262672), (-9.069090505045036, 6.161610277637618), (-8.760073510670088, 7.112666793932771), (-8.760073510670086, 8.112666793932771), (-9.069090505045033, 9.063723310227925), (-9.656875757337504, 9.872740304602875), (-10.465892751712454, 10.460525556895348), (-11.416949268007608, 10.769542551270298), (-12.416949268007604, 10.769542551270298), (-13.368005784302758, 10.460525556895355), (-14.177022778677706, 9.87274030460288), (-14.764808030970181, 9.063723310227935), (-15.07382502534513, 8.11266679393278), (-15.073825025345132, 7.11266679393278), (-14.764808030970185, 6.161610277637626), (-14.177022778677713, 5.352593283262678), (-13.368005784302767, 4.764808030970203), (-14.368005784302767, 4.7648080309702046), (-15.319062300597924, 4.455791036595256), (-16.12807929497287, 3.8680057843027846), (-16.715864547265344, 3.05898878992784), (-17.024881541640294, 2.107932273632687), (-17.024881541640294, 1.107932273632686), (-16.715864547265348, 0.15687575733753223), (-16.128079294972878, -0.6521412370374163), (-15.31906230059793, -1.2399264893298905), (-14.368005784302778, -1.5489434837048395), (-13.368005784302776, -1.548943483704841), (-12.416949268007622, -1.2399264893298958), (-13.004734520300097, -2.048943483704843), (-13.313751514675044, -3), (-13.313751514675044, -4), (-13.004734520300097, -4.951056516295151), (-12.416949268007627, -5.760073510670098), (-11.607932273632677, -6.347858762962572), (-10.656875757337524, -6.65687575733752), (-9.656875757337524, -6.656875757337522), (-8.705819241042372, -6.347858762962572), (-7.896802246667421, -5.760073510670102), (-7.309016994374949, -4.951056516295154), (-7, -4), (-2, -4), (-2, -9), (-1, -9), (-1, -2), (1, -2), (1, -3), (2, -3), (2, -2), (3, -2), (3, -4), (4, -4), (4, -2), (5, -2), (5, -3), (6, -3), (6, -2), (8, -2), (8, -1), (2, -1), (2, 0), (3, 0), (3, 1), (2, 1), (2, 2), (9, 2), (9, 3), (8, 3), (8, 4), (9, 4), (9.309016994374947, 3.0489434837048464), (9.896802246667418, 2.239926489329898), (10.705819241042366, 1.6521412370374255), (11.65687575733752, 1.3431242426624745), (10.705819241042366, 1.0341072482875255), (9.89680224666742, 0.44632199599505107), (9.309016994374948, -0.3626949983798975), (9, -1.3137515146750518), (9, -2.313751514675052), (9.309016994374954, -3.2648080309702046), (9.896802246667427, -4.073825025345151), (8.945745730372273, -3.7648080309702032), (7.945745730372273, -3.764808030970203), (6.99468921407712, -4.073825025345149), (6.185672219702171, -4.661610277637623), (5.597886967409697, -5.470627272012569), (5.28886997303475, -6.421683788307723), (5.28886997303475, -7.421683788307723), (4.701084720742278, -6.612666793932774), (3.8920677263673324, -6.024881541640299), (2.9410112100721792, -5.715864547265349), (1.9410112100721788, -5.715864547265347), (0.9899546937770243, -6.024881541640292), (0.18093769940207594, -6.612666793932764), (-0.4068475528903992, -7.42168378830771), (-0.7158645472653493, -8.372740304602862), (-0.7158645472653502, -9.372740304602866), (-0.4068475528904054, -10.32379682089802), (0.1809376994020644, -11.132813815272966), (0.9899546937770101, -11.720599067565441), (-0.010045306222990158, -11.72059906756544), (-0.9611018225181442, -12.029616061940384), (-1.7701188168930932, -12.617401314232856), (-1.4611018225181427, -11.666344797937702), (-1.4611018225181396, -10.666344797937704), (-1.7701188168930841, -9.715288281642549), (-2.3579040691855546, -8.906271287267598), (-3.1669210635605003, -8.318486034975123), (-4.117977579855653, -8.009469040600173), (-5.117977579855653, -8.00946904060017), (-6.0690340961508085, -8.318486034975113), (-6.878051090525759, -8.906271287267586), (-7.465836342818234, -9.715288281642533), (-7.774853337193184, -10.666344797937683), (-7.774853337193187, -11.666344797937683), (-7.465836342818243, -12.617401314232838), (-6.878051090525773, -13.426418308607786), (-6.069034096150826, -14.014203560900263), (-5.117977579855673, -14.323220555275213), (-4.1179775798556735, -14.323220555275217), (-3.166921063560519, -14.014203560900272), (-2.357904069185569, -13.426418308607802), (-2.6669210635605185, -14.377474824902954), (-2.6669210635605207, -15.377474824902954), (-2.3579040691855733, -16.328531341198108), (-1.770118816893107, -17.137548335573058), (-0.9611018225181556, -17.725333587865535), (-0.010045306223002592, -18.034350582240485), (0.9899546937769977, -18.034350582240485), (1.9410112100721468, -17.725333587865542), (2.7500282044471005, -17.13754833557307), (3.3378134567395716, -16.328531341198122), (3.646830451114525, -15.377474824902968), (3.646830451114524, -14.37747482490297), (3.3378134567395814, -13.426418308607815), (2.7500282044471085, -12.617401314232866), (1.9410112100721664, -12.029616061940391), (2.941011210072162, -12.029616061940393), (3.8920677263673205, -11.720599067565448), (4.701084720742266, -11.132813815272975), (5.288869973034744, -10.32379682089803), (5.597886967409692, -9.372740304602878), (5.597886967409696, -8.372740304602878), (6.185672219702171, -9.181757298977827), (6.994689214077118, -9.7695425512703), (7.945745730372272, -10.078559545645248), (8.945745730372266, -10.078559545645248), (9.896802246667425, -9.7695425512703), (10.70581924104237, -9.181757298977828), (11.293604493334847, -8.37274030460288), (11.602621487709792, -7.4216837883077265), (11.602621487709795, -6.4216837883077265), (11.293604493334847, -5.470627272012574), (10.705819241042375, -4.6616102776376245), (11.65687575733753, -4.970627272012569), (12.656875757337536, -4.9706272720125675), (13.607932273632684, -4.661610277637619), (14.416949268007631, -4.073825025345146), (15.0047345203001, -3.2648080309701966), (15.313751514675047, -2.3137515146750425), (15.313751514675047, -1.313751514675042), (15.004734520300097, -0.36269499837988817), (14.416949268007624, 0.44632199599505806), (13.607932273632674, 1.0341072482875302), (12.656875757337522, 1.3431242426624768), (13.607932273632674, 1.6521412370374202), (14.416949268007624, 2.239926489329898), (15.004734520300097, 3.0489434837048393), (15.313751514675044, 4), (15.313751514675044, 5), (15.004734520300097, 5.951056516295152), (14.416949268007627, 6.760073510670099), (13.607932273632677, 7.347858762962573), (12.656875757337524, 7.656875757337521), (11.656875757337524, 7.656875757337521), (10.70581924104237, 7.347858762962574), (9.896802246667422, 6.760073510670101), (9.309016994374948, 5.951056516295154), (9, 5), (8, 5), (8, 6), (7, 6), (7, 3), (6, 3), (6, 6), (5, 6), (5, 3), (4, 3), (4, 9), (4.951056516295154, 9.309016994374947), (5.760073510670102, 9.89680224666742), (6.347858762962575, 10.705819241042366), (6.656875757337524, 11.65687575733752), (6.656875757337524, 12.65687575733752), (6.347858762962577, 13.607932273632674), (5.760073510670104, 14.416949268007622), (4.951056516295157, 15.004734520300097), (4, 15.313751514675044), (3, 15.313751514675044), (2.048943483704849, 15.004734520300097), (1.2399264893299007, 14.416949268007627), (0.6521412370374278, 13.607932273632677), (0.34312424266247943, 12.656875757337524), (0.343124242662479, 11.656875757337524), (0.6521412370374269, 10.70581924104237), (1.239926489329899, 9.896802246667422), (2.0489434837048464, 9.309016994374948), (3, 9), (3, 8), (1, 8), (1, 7), (3, 7), (3, 3), (2, 3), (2, 6), (1, 6), (1, 5), (0, 5), (0, 12), (-1, 12), (-1, 11), (-2, 11), (-2, 13), (-3, 13), (-3, 11), (-4, 11), (-4, 18), (-5, 18), (-5, 17), (-7, 17), (-7, 16), (-5, 16), (-5, 11), (-8, 11), (-8, 10), (-1, 10), (-1, 5), (-2, 5), (-2, 8), (-3, 8), (-3, 5), (-5, 5), (-5, 4), (-4, 4), (-4, 3), (-9, 3), (-9, 2), (-4, 2), (-4, 0), (-3, 0), (-3, 4), (1, 4), (1, 3), (-1, 3), (-1, 2), (1, 2), (1, 1), (0, 1), (0, 0), (1, 0), (1, -1), (-3, -1), (-3, -2), (-2, -2), (-2, -3), (-7, -3), (-7.309016994374947, -2.048943483704846), (-7.89680224666742, -1.2399264893298985)))

  val enterprise = FPolygon(Seq((152,26), (151,20), (274,16), (280,21), (368,28), (366,35), (256,43), (316,135), (375,135), (386,128), (406,130), (412,125), (407,117), (432,85), (437,86), (500,63), (559,60), (629,78), (691,114), (690,211), (629,246), (559,262), (500,264), (437,238), (433,239), (408,208), (413,199), (407,196), (386,196), (376,190), (316,187), (256,281), (366,289), (368,295), (280,302), (274,308), (151,315), (152,301), (59,295), (59,289), (232,282), (250,229), (140,225), (140,219), (196,215), (237,179), (186,172), (181,165), (181,161), (186,154), (237,146), (196,108), (140,105), (140,98), (250,96), (232,43), (59,34), (59,29)))
}
