package conc

import conc.Chapter3.Ex5.PureLazyCell

object PureLazyCellTest extends App {
  def initialization = {
    log("calculation ...")
    Thread.sleep(1000)
    s"result (calculate by ${Thread.currentThread().getName})"
  }

  val p = new PureLazyCell[String](initialization)

  log("start")

  val t = (1 to 10).map((i) => thread {
    val sleep = (Math.random * 10000).toInt
    Thread.sleep(sleep)

    (1 to 3).foreach((i) => log(s"v$i = ${p.apply}"))
  })

  t.foreach(_.join)
}
