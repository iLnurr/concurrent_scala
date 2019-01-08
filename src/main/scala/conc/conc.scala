import java.util.{Timer, TimerTask}

import eu.timepit.refined.api.{Refined, RefinedTypeOps}
import eu.timepit.refined.boolean.And
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.string.Url

package object conc {
  def thread(body: => Unit): Thread = {
    val t = new Thread {
      override def run(): Unit = body
    }
    t.start()
    t
  }

  def log(msg: String): Unit = println(s"${Thread.currentThread().getName} $msg")

  def mytimer(f: Unit, delay: Long = 0, period: Long = 50): Timer = {
    val timer = new Timer()

    timer.schedule(
      new TimerTask {
        def run() = f
      },
      delay,
      period
    )
    timer
  }

  @volatile var dummy: Any = _
  def timed[T](body: => T): Double = {
    val start = System.nanoTime
    dummy = body
    val end = System.nanoTime
    ((end - start) / 1000) / 1000.0
  }

  def warmedTimed[T](n: Int = 200)(body: =>T): Double = {
    for (_ <- 0 until n) body
    timed(body)
  }

  type RefinedUrl = String Refined And[NonEmpty, Url]
  object RefinedUrl extends RefinedTypeOps[RefinedUrl, String]
}