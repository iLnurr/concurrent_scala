package conc

import Chapter7._

import scala.concurrent.Future
import scala.concurrent.stm.{InTxn, Ref}
import scala.util.Random
import scala.concurrent.ExecutionContext.Implicits.global

object Ch7Ex4Test extends App {
  //test
  val r = Ref(10)

  def block(txn: InTxn): Int = {
    var x: Int = r.get(txn)
    Thread.sleep(10)
    x += Random.nextInt(100)
    r.set(x)(txn)
    x
  }

  (1 to 100).map(i =>
    Future {
      try {
        atomicWithRetryMax[Int](3)(block)
        log(s"Transaction: $i - ok")
      } catch {
        case RetriesException(cntRetries) => log(s"Transaction: $i (retries = $cntRetries)")
      }
    }
  )

  Thread.sleep(3000)
}
