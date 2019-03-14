package conc

import Chapter7._

import scala.concurrent.{Await, Future}
import scala.concurrent.stm._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object Ch7Ex5Test extends App {
  val result = warmedTimed(20000)(test)

  log(result.toString)

  def test: () => Unit = () => {
    //test
    val tQueue = new TQueue[Integer]

    val l = 1 to 20

    val deqF = Future {
      l.map { i =>
        atomic { implicit txn =>
          val x = tQueue.dequeue

          Txn.afterCommit { _ =>
            log(s"dequeu: $x")
          }
          x
        }
      }
    }

    val enqF = Future {
      l.map { i =>
        atomic { implicit txn =>
          tQueue.enqueue(i)

          Txn.afterCommit { _ =>
            log(s"enque: $i")
          }
          i
        }
      }
    }

    val res = for {
      enq <- enqF
      deq <- deqF
    } yield {
      assert(deq.size == enq.size)
    }

    Await.result(res, 10.seconds)
  }
}
