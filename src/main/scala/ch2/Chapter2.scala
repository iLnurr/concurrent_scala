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
      tasks -= priorTask
      priorTask._1
    }
    override def run() = while (true) {
      val task = poll()
      task()
    }
  }
  Worker.start()
  def asynchronous(priority: Int)(body: =>Unit) = tasks.synchronized {
    tasks += (() => body, priority)
    tasks.notify()
  }
  asynchronous(1) { log("Hello ") }
  asynchronous(5) { log("World!") }
  Thread.sleep(500)
}