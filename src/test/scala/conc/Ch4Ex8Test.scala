package conc

import scala.concurrent.{Future, Promise}
import scala.concurrent.ExecutionContext.Implicits.global
import Chapter4.Ex8._

object Ch4Ex8Test extends App {
  val pT = Promise[String]
  val pS: Promise[Int] = pT.compose((s) => s"val = $s")

  Future {
    Thread.sleep(1000)
    pS.success(1)
    //    pS.failure(new Exception)
  }



  pT.future foreach {
    case s => log(s)
  }

  pT.future.failed foreach { case t => log(s"q failed with $t") }


  Thread.sleep(2000)


}
