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
        log(s"get2 = ${value}")
        empty = true
        value.get
      }
    }

    def put(x: T): Unit = this.synchronized {
      if (!empty) {
        throw new RuntimeException("non empty")
      } else {
        log(s"put2 = ${x}")
        value = Some(x)
        empty = false
      }
    }
  }

  val syncVar2 = new SyncVar2[Int]()

  val producer2 = thread {
    var x = 0
    while (x < 15) {
      if (syncVar2.isEmpty) {
        syncVar2.put(x)
        x += 1
      }
    }
  }

  val consumer2 = thread {
    while (true) if (syncVar2.nonEmpty) syncVar2.get()
  }

  producer2.join()
  consumer2.join()

  /** #5 */
  class SyncVar3[T] {
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

    def getWait(): T = this.synchronized {
      while (empty) {
        this.wait()
      }
      empty = true
      this.notify()
      log(s"get3 =  ${value}")
      value.get
    }

    def putWait(x: T): Unit = this.synchronized {
      while (!empty) {
        this.wait()
      }
      empty = false
      this.notify()
      log(s"put3 = ${x}")
      value = Some(x)
    }
  }

  val syncVar3 = new SyncVar3[Int]()

  val producer3 = thread {
    var x = 0
    while (x < 15) {
      syncVar3.putWait(x)
      x += 1
    }
  }

  val consumer3 = thread {
    while (true) syncVar3.getWait()
  }

  producer3.join()
  consumer3.join()


}
