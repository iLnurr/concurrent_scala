package ch2

import PriorityTaskPoolExtended._
import PriorityTaskPoolWithImportant._

object PriorityTaskPoolTest  extends App {
  val range = 1 to 1000

  val pool = new PriorityTaskPoolExtended(5)

  range.foreach { prior =>
    pool.asynchronous(prior)(log(s"Task with priority:$prior "))
  }

  Thread.sleep(5000)

  val poolWithImportant = new PriorityTaskPoolImportant(5, 800)

  range.foreach { prior =>
    poolWithImportant.asynchronous(prior)(log(s"Task with priority and important:$prior "))
  }

  Thread.sleep(5000)

}
