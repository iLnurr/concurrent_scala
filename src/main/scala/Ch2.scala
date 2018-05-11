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

  /** #1 */
  def parallel[A,B] (a: => A, b: => B): (A,B) = {
    var aV: Option[A] = None
    var bV: Option[B] = None

    val t1 = thread {
      aV = Some(a)
      log(s"${aV.toString}")
    }
    val t2 = thread {
      bV = Some(b)
      log(s"${bV.toString}")
    }
    t1.join()
    t2.join()

    (aV.get, bV.get)
  }

  val tuple = parallel("aa", "bb")

  /** #2 */
  def periodically(duration: Long)(block: => Unit) = {
    val worker = new Thread() {
      override def run(): Unit = {
        while (true) {
          log("sleep")
          Thread.sleep(duration)
          block
        }
      }
    }

    worker.start()

  }

//  periodically(1000)(log("work "))

  /** #3 */
  class SyncVar1[T] {
    var isEmpty = true
    var value: Option[T] = None
    def get(): T = this.synchronized {
      if (isEmpty) {
        throw new RuntimeException("is empty")
      } else {
        isEmpty = true
        value.get
      }
    }

    def put(x: T): Unit = this.synchronized {
      if (!isEmpty) {
        throw new RuntimeException("non empty")
      } else {
        value = Some(x)
        isEmpty = false
      }
    }
  }

  /** #4 */
  class SyncVar2[T] {
    private var empty = true
    private var value: Option[T] = None

    def isEmpty: Boolean = synchronized(empty)
    def nonEmpty: Boolean = synchronized(!empty)
    def get(): T = this.synchronized {
      if (empty) {
        throw new RuntimeException("is empty")
      } else {
        log(s"get ${value}")
        empty = true
        value.get
      }
    }

    def put(x: T): Unit = this.synchronized {
      if (!empty) {
        throw new RuntimeException("non empty")
      } else {
        log(s"put ${x}")
        value = Some(x)
        empty = false
      }
    }
  }

  val syncVar = new SyncVar2[Int]()

  val producer = thread {
    var x = 0
    while (x < 15) {
      if (syncVar.isEmpty) {
        syncVar.put(x)
        x += 1
      }
    }
  }

  val consumer = thread {
    while (true) if (syncVar.nonEmpty) syncVar.get()
  }

  producer.join()
  consumer.join()
}
