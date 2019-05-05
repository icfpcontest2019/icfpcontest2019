package lambda.contest

/**
  * @author Ilya Sergey
  */
object ContestErrorMessages {

  val MALFORMED_TASK = "Malformed task description."
  val MALFORMED_ROOM = "The room polygon is ill-formed (e.g., it has self-intersections)."
  val NON_RECTILINEAR_ROOM = "The room is not rectilinear."
  val BAD_INIT_POS = "Initial watchman position is not within the room."
  val BOUNDING_BOX = "Room must be within non-negative bounding box."
  val MALFORMED_OBSTACLE = "Some obstacles are not well-formed (non-rectilinear, have self-intersections, etc.)."
  val INIT_POS_OBSTACLE = "Initial position is within an obstacle."
  val OBSTACLE_NOW_WITHIN = "Some obstacles are not fully within the room or touch its walls."
  val OBSTACLES_INTERSECT = "Some obstacles intersect."
  val BOOSTER_NOT_IN_ROOM = "A booster not in the room."
  val BOOSTER_IN_OBSTACLE = "A booster is in an obstacle."
  val BOOSTER_CLASH = "Two or more boosters in the same position."


  val CELL_DOES_NOT_HAVE_SPACE  = "Cell is not a part of the room interior."
  val CANNOT_COLLECT_BOOSTER = "Cannot collect booster from the non-free cell."
  val CALL_POINT_INSTALLED = "Cannot perform this operation in this location."
  val TELEPORT_INSTALLED = "Cannot perform this operation in this location."

  val WATCHMAN_NOT_FOUND = "Watchman not found."
  val BAD_ACTION = "Bad route action taken; interrupting validation."
  val BAD_BOOSTER = "No such booster exists; interrupting validation."
  val NO_BOOSTER = "No such booster available; interrupting validation."
  val BAD_TELEPORT_LOCATION = "Cannot teleport to this location."
  val CANNOT_CALL_FRIEND = "Cannot perform this operation in this location."
  val BAD_BATTERY_POSITION = "Cannot attach battery at this position."


}
