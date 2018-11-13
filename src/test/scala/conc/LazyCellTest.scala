package conc

import conc.Chapter3.Ex4.LazyCell

object LazyCellTest extends App {
  def func = {
    log("start...")
    Thread.sleep(10000)
    s"Calculation by ${Thread.currentThread().getName}"
  }

  val a = new LazyCell[String](func)

  log("Start")

  (0 to 50).
    map((i) => thread({
      Thread.sleep((Math.random * 10).toInt)
      println(a.apply)
    })).
    foreach(_.join)
}
