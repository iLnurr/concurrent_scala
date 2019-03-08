package conc

import java.util.concurrent.atomic.AtomicInteger

import conc.Chapter7._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.stm._

object Ch7Ex2 extends App {
  //test
  val mVar = new MVar[Integer]

  val l = 1 to 1001

  l.map(
    i => Future {
      atomic { implicit txn =>
        mVar.put(i)
      }
    }
  )

  val sum = new AtomicInteger(0)

  l.foreach(_=> {
    atomic { implicit txn =>
      val i = mVar.take
      Txn.afterCommit(_ => sum.addAndGet(i))
    }
  })

  Thread.sleep(5000)

  if (l.sum != sum.get) log(s"Error !!!! ${l.sum} != $sum")

  log(s"summ = ${sum.get}")

  //test swap
  log("--- test swap ------------")

  val mva = new MVar[String]
  val mvb = new MVar[String]
  atomic {implicit txn =>
    mva.put("a")
    mvb.put("b")
  }

  l.map(i =>
    Future{
      atomic {implicit txn =>
        swap(mva, mvb)
      }
    }
  )

  Thread.sleep(5000)

  atomic {implicit txn =>
    val a = mva.take
    val b = mvb.take

    Txn.afterCommit( _ =>  log(s"a= $a, b = $b"))
  }
}
