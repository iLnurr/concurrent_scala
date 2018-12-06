import java.util.{Timer, TimerTask}

import eu.timepit.refined.api.{Refined, RefinedTypeOps}
import eu.timepit.refined.boolean.And
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.string.Url

import scala.concurrent.{ExecutionContext, Future}

package object conc {
  def thread(body: => Unit) = {
    val t = new Thread {
      override def run(): Unit = body
    }
    t.start()
    t
  }

  def log(msg: String): Unit = println(s"${Thread.currentThread().getName} $msg")

  def mytimer(f: Unit, delay: Long = 0, period: Long = 50) = {
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

  type RefinedUrl = String Refined And[NonEmpty, Url]
  object RefinedUrl extends RefinedTypeOps[RefinedUrl, String]
}