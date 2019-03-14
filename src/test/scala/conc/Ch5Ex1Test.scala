package conc

import Chapter5._

object Ch5Ex1Test extends App {
  val index = 1000000
  val all = (1 to index).map { _ =>
    val l = timed(() ⇒ randomLong)
    println(l)
    l
  }
  val avg = all.seq.sum / index
  println()
  println(s"avg = $avg")


  object Timed {

    @volatile
    var dummy: Any = _

    def buildObjects(count:Int) = {
      var i = 0
      val start = System.nanoTime
      while (i < count) {
        dummy = new Object
        i += 1
      }
      (System.nanoTime - start)/count.toDouble
    }

  }

  var i = 0
  var summ = 0D

  var timePrev = 0D
  while (i < 30) {

    val time = Timed.buildObjects(10000000)
    val e = Math.abs(time - timePrev)/time*100

    //check steady state
    if (e < 10) {
      i += 1
      summ += time
    } else {
      i = 0
      summ = time
    }

    timePrev = time
    log(s"time = ${time.toString} e = ${Math.round(e)}, i = $i")

  }

  log("----------------------------------------------------")
  log(s"avg = ${summ/(i+1)} nanoseconds")

}
