import java.util.concurrent._

import lambda.geometry.GeometryException

package object lambda {

  /**
    * Prevent the function from executing too much time
    *
    * @param time Time in millisecond
    * @param aux additional value to put in exception
    */
  def runWithTimer[T](task: () => T, time: Int, loc: String, aux: Any): T = {

    val executor: ExecutorService = Executors.newSingleThreadExecutor()
    val future = executor.submit(new Callable[T] {
      override def call() = task()
    })
    try {
      // retrurn result
      future.get(time, TimeUnit.MILLISECONDS)
    } catch {
      case e: TimeoutException =>
        future.cancel(true)
        throw GeometryException(loc, aux)
    }
  }


}
