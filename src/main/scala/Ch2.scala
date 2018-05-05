
object Ch2 extends App {
  def thread(body: => Unit) = {
    val t = new Thread {
      override def run(): Unit = body
    }
    t.start()
    t
  }

  def log(msg: String): Unit = println(s"${Thread.currentThread().getName} $msg")

  def threadWithResult[T](body: => T) = {
    var result: Option[T] = None
    val thread = new Thread {
      override def run(): Unit = {
        result = Some(body)
      }
    }
    thread.start()
    println(s"try to run ${body} on ${thread}")
    thread.join()
    result
  }

  def parallel[A,B] (a: => A, b: => B): (A,B) = {
    val resA = threadWithResult(a)
    val resB = threadWithResult(b)
    val maybePair = for {
      a <- resA
      b <- resB
    } yield (a,b)

    assert(maybePair.isDefined, "Both result must be defined")

    maybePair.get
  }

  val tuple = parallel("aa", "bb")

  def periodically(duration: Long)(b: => Unit) = {
    val worker = new Thread() {
      override def run(): Unit = {
        while (true) {
          log("sleep")
          Thread.sleep(duration)
          b
        }
      }
    }

    worker.start()

  }

  periodically(1000)(log("work "))
}
