package conc

import java.util.concurrent.atomic.AtomicInteger

import scala.concurrent.stm.Txn.RolledBack
import scala.concurrent.stm._

/** https://nbronson.github.io/scala-stm/quick_start.html */
object Chapter7 {
  /**
    * Реализуйте абстракцию транзакционной пары в виде класса TPair
    * Помимо методов чтения/записи для двух полей, транзакционная пара
    * должна поддерживать метод swap , меняющий значения полей местами и ко-
    * торый может вызываться, только если типы P и Q совпадают.
    */
  class TPair[P, Q](pinit: P, qinit: Q) {
    private val refP = Ref(pinit)
    private val refQ = Ref(qinit)
    def first(implicit txn: InTxn): P = refP.apply()
    def first_=(x: P)(implicit txn: InTxn): P = refP.swap(x)
    def second(implicit txn: InTxn): Q = refQ.apply()
    def second_=(x: Q)(implicit txn: InTxn): Q = refQ.swap(x)
    def swap()(implicit e: P =:= Q, txn: InTxn): Unit = {
      val old = first
      first = second.asInstanceOf[P]
      second = e(old)
    }
  }

  /**
    * Реализуйте с помощью ScalaSTM абстракцию изменяемой переменной из
    * языка Haskell в виде класса MVar
    * class MVar[T] {
    *   def put(x: T)(implicit txn: InTxn): Unit = ???
    *   def take()(implicit txn: InTxn): T = ???
    * }
    * Объект MVar может быть пустым или полным. Вызов метода put для полного
    * экземпляра MVar должен блокироваться, пока тот не опустеет, и затем добав-
    * лять элемент. Аналогично вызов take для пустого экземпляра MVar должен
    * блокироваться, пока тот не станет полным, и затем удалять и возвращать
    * элемент. Справившись с этим заданием, реализуйте метод swap , принимаю-
    * щий два объекта MVar и меняющий их значения местами:
    * def swap[T](a: MVar[T], b: MVar[T])(implicit txn: InTxn)
    * Сравните класс MVar с классом SyncVar из главы 2 «Конкуренция в JVM и модель
    * памяти в Java». Можно ли реализовать метод swap для объектов SyncVar без
    * изменения внутренней реализации класса?
    */

  class MVar[T] {
    private val ref = Ref[Option[T]](None)
    def put(x: T)(implicit txn: InTxn): Unit = ref.get match {
      case Some(_) => retry
      case None => {
        ref.set(Some(x))
      }
    }
    def take()(implicit txn: InTxn): T = ref.get match {
      case Some(result) => {
        ref.set(None)
        result
      }
      case None => retry
    }
  }

  def swap[T](a: MVar[T], b: MVar[T])(implicit txn: InTxn): Unit = {
    val old = a.take
    a.put(b.take())
    b.put(old)
  }

  /**
    * Реализуйте метод atomicRollbackCount , который определяет, сколько откатов
    * было выполнено в ходе выполнения транзакции, прежде чем она завершилась успехом:
    * def atomicRollbackCount[T](block: InTxn => T): (T, Int)
    */

  def atomicRollbackCount[T](block: InTxn => T): (T, Int) = {
    val counter = new AtomicInteger(0)
    atomic { implicit txn =>
      Txn.afterRollback { case RolledBack(_) => counter.addAndGet(1) }
      block(txn) -> counter.get()
    }
  }

  /**
    * Реализуйте метод atomicWithRetryMax,
    * запускающий транзакцию с ограниченным количеством повторений (не больше n раз):
    *
    * def atomicWithRetryMax[T](n: Int)(block: InTxn => T): T
    *
    * По достижении максимального числа повторений должно возбуждаться исключение.
    */

  case class RetriesException(n: Int) extends RuntimeException(n.toString)
  def atomicWithRetryMax[T](n: Int)(block: InTxn => T): T = {
    val retries = new AtomicInteger(0)
    atomic { implicit txn =>
      Txn.afterRollback(_ => retries.incrementAndGet())
      if (retries.get() > n) throw RetriesException(n)
      block(txn)
    }
  }

}
