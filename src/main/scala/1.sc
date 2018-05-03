def thread(body: => Unit) = {
  val t = new Thread {
    override def run(): Unit = body
  }
  t.start()
  t
}

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
  (resA.get, resB.get)
}

val tuple = parallel("aa", "bb")