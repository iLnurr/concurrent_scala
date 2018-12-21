package conc

import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global

object Ch4Ex345 extends App {
  def p(i: Int) = i > 0
  def f1 = Future { 100 }
  def f2 = Future { -100 }
  def f3 = Future { throw new Exception("Error")}

   {
     //test Ex3
     import Chapter4.Ex3._

     log("f1 = " + Await.result(f1.exists(p), Duration.Inf).toString)
     log("f2 = " + Await.result(f2.exists(p), Duration.Inf).toString)
     log("f3 = " + Await.result(f3.exists(p), Duration.Inf).toString)
   }

  {
    //test Ex4
    import Chapter4.Ex4._

    log("f1 = " + Await.result(f1.exists(p), Duration.Inf).toString)
    log("f2 = " + Await.result(f2.exists(p), Duration.Inf).toString)
    log("f3 = " + Await.result(f3.exists(p), Duration.Inf).toString)
  }

  {
    //test Ex5
    import Chapter4.Ex5._

    log("f1 = " + Await.result(f1.exists(p), Duration.Inf).toString)
    log("f2 = " + Await.result(f2.exists(p), Duration.Inf).toString)
    log("f3 = " + Await.result(f3.exists(p), Duration.Inf).toString)
  }



}
