package conc

import Chapter7._

import scala.concurrent.Future
import scala.concurrent.stm._
import scala.concurrent.ExecutionContext.Implicits.global

object Ch7Ex1 extends App  {
  val p = new TPair[String,String]("first value","second value")

  def swapOne = atomic { implicit txn =>
    p.swap

    val vF = p.first
    val vS = p.second

    Txn.afterCommit { _ =>
      assert(vS != vF)
    }
  }

  (1 to 1001).map(_ => Future {
    swapOne
  })

  Thread.sleep(2000)

  atomic {implicit txn =>
    log(s"Result: first = '${p.first}' vSecond = '${p.second}'")
  }

}
