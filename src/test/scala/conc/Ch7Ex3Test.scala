package conc

import Chapter7._

import scala.concurrent.Future
import scala.concurrent.stm.{InTxn, Ref}
import scala.util.Random
import scala.concurrent.ExecutionContext.Implicits.global

object Ch7Ex3Test extends App {
  //test
  val r = Ref(10)

  def block(txn: InTxn): Int = {
    var x: Int = r.get(txn)
    x = Random.nextInt(10000)
    Thread.sleep(10)
    r.set(x)(txn)
    x
  }

  (1 to 100).map(i =>
    Future {
      atomicRollbackCount[Int](block) match {
        case (_, cnt) => log(s"Transaction: $i, retries = $cnt")
        case _ => log("???")
      }
    }
  )

  Thread.sleep(3000)
}
