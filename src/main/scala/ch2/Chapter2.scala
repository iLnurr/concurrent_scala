package ch2

import scala.collection.mutable

object Chapter2 {
  object first {
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

    parallel("aa", "bb")
  }

  object sec {
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

    periodically(1000)(log("work "))
  }

  object third {
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

  }

  object frth {
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
  }

  object fifth {
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

}

object SynchronizedProtectedUid {
  var uidCount = 0L
  def getUniqueId() = this.synchronized {
    val freshUid = uidCount + 1
    uidCount = freshUid
    freshUid
  }
}


object Accounts {
  /**
    * The send method in the Deadlocks section was used to transfer money between
    * the two accounts. The sendAll method takes a set accounts of bank accounts
    * and a target bank account, and transfers all the money from every account in
    * accounts to the target bank account. The sendAll method has the following
    * signature:
    * def sendAll(accounts: Set[Account], target: Account): Unit
    * Implement the sendAll method and ensure that a deadlock cannot occur.
    */
  import SynchronizedProtectedUid.getUniqueId
  class Account(val name: String, var money: Int) {
    val uid = getUniqueId()
  }

  def send(a1: Account, a2: Account, n: Int) {
    def adjust() {
      a1.money -= n
      a2.money += n
    }
    if (a1.uid < a2.uid)
      a1.synchronized { a2.synchronized { adjust() } }
    else a2.synchronized { a1.synchronized { adjust() } }
  }

  def sendAll(accounts: Set[Account], target: Account): Unit = {
    accounts.foldLeft(target){ (targetAcc, rethd) =>
      send(rethd, targetAcc, rethd.money)
      targetAcc
    }
  }
}

object SynchronizedPool extends App {
  private val tasks = mutable.Queue[() => Unit]()
  object Worker extends Thread {
    setDaemon(true)
    def poll() = tasks.synchronized {
      while (tasks.isEmpty) tasks.wait()
      tasks.dequeue()
    }
    override def run() = while (true) {
      val task = poll()
      task()
    }
  }
  Worker.start()
  def asynchronous(body: =>Unit) = tasks.synchronized {
    tasks.enqueue(() => body)
    tasks.notify()
  }
  asynchronous { log("Hello ") }
  asynchronous { log("World!") }
  Thread.sleep(500)
}

object PriorityTaskPool extends App {
  /**
    * Recall the asynchronous method from the Guarded blocks section. This method
    * stores the tasks in a First In First Out (FIFO) queue; before a submitted task is
    * executed, all the previously submitted tasks need to be executed. In some cases,
    * we want to assign priorities to tasks so that a high-priority task can execute as
    * soon as it is submitted to the task pool. Implement a PriorityTaskPool class
    * that has the asynchronous method with the following signature:
    * def asynchronous(priority: Int)(task: =>Unit): Unit
    * A single worker thread picks tasks submitted to the pool and executes them.
    * Whenever the worker thread picks a new task from the pool for execution,
    * that task must have the highest priority in the pool.
    */
  private val tasks = mutable.ArrayBuffer[(() => Unit, Int)]()
  object Worker extends Thread {
    setDaemon(true)
    def poll() = tasks.synchronized {
      while (tasks.isEmpty) tasks.wait()
      val priorTask = tasks.maxBy(_._2)
      val (task, priority) = priorTask
      tasks -= priorTask
      task
    }
    override def run() = while (true) {
      val task = poll()
      task()
    }
  }
  Worker.start()
  def asynchronous(priority: Int)(body: =>Unit): Unit = tasks.synchronized {
    tasks += (() => body, priority)
    tasks.notify()
  }
  asynchronous(1) { log("Hello ") }
  asynchronous(5) { log("World!") }
  Thread.sleep(500)
}

object PriorityTaskPoolExtended extends App {
  /**
    * Extend the PriorityTaskPool class from the previous exercise so that it
    * supports any number of worker threads p . The parameter p is specified in the
    * constructor of the PriorityTaskPool class.
    */
  class PriorityTaskPoolExtended(p: Int) {
    private val tasks = mutable.ArrayBuffer[(() => Unit, Int)]()

    class Worker extends Thread {
      setDaemon(true)
      def poll() = tasks.synchronized {
        while (tasks.isEmpty) tasks.wait()
        val priorTask = tasks.maxBy(_._2)
        val (task, priority) = priorTask
        tasks -= priorTask
        task
      }
      override def run() = while (true) {
        val task = poll()
        task()
      }
    }

    def createWorkers(): Unit = {
      (1 to p).foreach(_ => new Worker().start())
    }
    createWorkers()

    def asynchronous(priority: Int)(body: =>Unit): Unit = tasks.synchronized {
      tasks += (() => body, priority)
      tasks.notify()
    }
  }
}

object PriorityTaskPoolWithImportant extends App {
  /**
    * Extend the PriorityTaskPool class from the previous exercise so that it
    * supports the shutdown method:
    * def shutdown(): Unit
    * When the shutdown method is called, all the tasks with the priorities greater
    * than important must be completed, and the rest of the tasks must be
    * discarded. The important integer parameter is specified in the constructor
    * of the PriorityTaskPool class.
    */
  class PriorityTaskPoolImportant(p: Int, important: Int) {
    private val tasks = mutable.ArrayBuffer[(() => Unit, Int)]()

    class Worker extends Thread {
      setDaemon(true)
      def poll() = tasks.synchronized {
        while (tasks.isEmpty) tasks.wait()
        val priorTask = tasks.maxBy(_._2)
        val (task, priority) = priorTask
        tasks -= priorTask
        if (priority >= important) task else () => ()
      }
      override def run() = while (true) {
        val task = poll()
        task()
      }
    }

    def createWorkers(): Unit = {
      (1 to p).foreach(_ => new Worker().start())
    }
    createWorkers()

    def asynchronous(priority: Int)(body: => Unit): Unit = tasks.synchronized {
      tasks += (() => body, priority)
      tasks.notify()
    }
  }
}

object ConcurrentBiMap extends App {
  /**
    * Implement a ConcurrentBiMap collection, which is a concurrent bidirectional
    * map. The invariant is that every key is mapped to exactly one value, and vice
    * versa. Operations must be atomic. The concurrent bidirectional map has the
    * following interface:
    * Make sure that your implementation prevents deadlocks from occurring in
    * the map.
    */

  class ConcurrentBiMap[K, V] {
    private val lock = Array()
    private val f = mutable.Map[K,V]()
    private val r = mutable.Map[V,K]()

    def put(k: K, v: V): Option[(K, V)] = lock.synchronized {
      f += (k -> v)
      r += (v -> k)
      Some((k,v))
    }
    def removeKey(k: K): Option[V] = lock.synchronized {
      f.get(k).map { v =>
        f -= k
        r -= v
        v
      }
    }
    def removeValue(v: V): Option[K] = lock.synchronized {
      r.get(v).map { k =>
        f -= k
        r -= v
        k
      }
    }

    def getValue(k: K): Option[V] = f.get(k)
    def getKey(v: V): Option[K] = r.get(v)
    def size: Int = lock.synchronized(f.size)
    def iterator: Iterator[(K, V)] = lock.synchronized {
      f.iterator
    }

    /**
      * Add a replace method to the concurrent bidirectional map from the previous
      * exercise. The method should atomically replace a key-value pair with another
      * key-value pair:
      */
    def replace(k1: K, v1: V, k2: K, v2: V): Unit = lock.synchronized {
      f.get(k1).map { v =>
        if (v == v1) {
          f -= k1
          r -= v
          f += (k2 -> v2)
          r += (v2 -> k2)
        }
      }
    }
  }
}

object Cache {
  /**
    * Implement a cache method, which converts any function into a memoized version of itself. The first time that the resulting function is called for any argument, it is called in the same way as the original function. However, the result is memoized, and subsequently invoking the resulting function with the same arguments must return the previously returned value:
    * Make sure that your implementation works correctly when the resulting function is called simultaneously from multiple threads.
    */
  class Cache[K,V] {
    private val map = mutable.Map[K ⇒ V, Vector[(K,V)]]()
    def cache(f: K ⇒ V): K ⇒ V = k ⇒ {
      map.synchronized {
        map.get(f) match {
          case Some(underlying) ⇒
            underlying.find {
              case (key, _) ⇒ key == k
            }.
              map(_._2).getOrElse(
                add(f, k, underlying)
              )
          case None ⇒
            add(f, k)
        }
      }
    }

    private def add(f: K ⇒ V, k: K, old: Vector[(K,V)] = Vector.empty[(K,V)]) = {
      val result = f(k)
      log(s"add f:$f and key:$k")
      map += (f → (old ++ Vector((k, result))))
      result
    }
  }
}