package ch2

import PriorityTaskPoolExtended._

object PriorityTaskPoolTest  extends App {
  val range = 1 to 1000

  val pool = new PriorityTaskPool(5)

  range.foreach { prior =>
    pool.asynchronous(prior)(log(s"Task with priority:$prior "))
  }

  Thread.sleep(5000)
}
