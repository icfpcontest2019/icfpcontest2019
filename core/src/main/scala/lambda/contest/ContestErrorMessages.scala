package lambda.contest

/**
  * @author Ilya Sergey
  */
object ContestErrorMessages {

  val MALFORMED_TASK = "malformed task description"
  val MALFORMED_ROOM = "task polygon is ill-formed (e.g., it has self-intersections)"
  val NON_RECTILINEAR_ROOM = "The block is not rectilinear"
  val BAD_INIT_POS = "Initial position is not within the block"
  val BOUNDING_BOX = "Block must be within non-negative bounding box"
  val MALFORMED_OBSTACLE = "Some obstacles are not well-formed (non-rectilinear, have self-intersections, etc.)."
  val INIT_POS_OBSTACLE = "Initial position is within an obstacle."
  val OBSTACLE_NOW_WITHIN = "Some obstacles are not fully within the block or touch its boundaries"
  val OBSTACLES_INTERSECT = "Some obstacles intersect."
  val BOOSTER_NOT_IN_ROOM = "A booster not in the block"
  val BOOSTER_IN_OBSTACLE = "A booster is in an obstacle."
  val BOOSTER_CLASH = "Two or more boosters in the same position."


  val CELL_DOES_NOT_HAVE_SPACE  = "Cell is not a part of the block interior."
  val CANNOT_COLLECT_BOOSTER = "Cannot collect booster from the non-free cell."
  val CALL_POINT_INSTALLED = "Cannot perform this operation in this location."
  val TELEPORT_INSTALLED = "Cannot perform this operation in this location."

  val WATCHMAN_NOT_FOUND = "Typer not found."
  val BAD_ACTION = "Non-allowed action performed at location"
  val BAD_BOOSTER = "No such booster exists, tried at location"
  val NO_BOOSTER = "No such booster available, tried at location"
  val BAD_TELEPORT_LOCATION = "Cannot move to this location:"
  val CANNOT_CALL_FRIEND = "Cannot perform this operation in location"
  val BAD_BATTERY_POSITION = "Cannot use this boosters with arguments"


  val BAD_BOOSTER_FORMAT = "Booster descriptor is malformed"
  val DUPLICATED_SOLUTION = "Duplicating solution file"
  val BAD_SOLUTION_FILE_NAME = "Ill-formed solution file name"
  val BAD_SOLUTION_FORMAT = "solution text is malformed"
  
  val BAD_TASK_MATRIX = "Ill-formed task matrix"
  val BAD_CHAIN_FILE = "Bad file with lambda-chain descriptions"
  
  val HAS_OBSTACLES_ERROR = "the task description should contain no obstacles" 
  val SPEC_ERROR = "the task does not conform to the current block specification" 
  val SPEC_BOOSTER_ERROR = "wrong booster configuration" 
  val POINTS_NOT_INSIDE_ERROR = "some inside-points are not inside" 
  val POINTS_NOT_OUTSIDE_ERROR = "some outside-points are not outside" 
  
}
