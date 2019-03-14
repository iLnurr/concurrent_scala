package conc

import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}

import scala.collection.immutable.Queue
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

  /**
    * Реализуйте транзакционную очередь FIFO (First In First Out – первым при-
    * шел, первым вышел) в виде класса TQueue :
    * class TQueue[T] {
    *   def enqueue(x: T)(implicit txn: InTxn): Unit = ???
    *   def dequeue()(implicit txn: InTxn): T = ???
    * }
    * Класс TQueue имеет реализацию, схожую с классом scala.collection.mutable.Queue,
    * с той лишь разницей, что вызов dequeue для пустой очереди должен
    * блокироваться до появления в ней хотя бы одного элемента.
    */

  class TQueue[T] {
    private val r = Ref[Queue[T]](Queue.empty[T])
    def enqueue(x: T)(implicit txn: InTxn): Unit = {
      r() = r() :+ x
    }

    def dequeue()(implicit txn: InTxn): T = {
      r().dequeueOption match {
        case None => retry
        case Some((x,q)) => {
          r() = q
          x
        }
      }
    }
  }

  /**
    * Используйте ScalaSTM для реализации потокобезопасного класса TArray­Buffer , наследующего интерфейс scala.collection.mutable.Buffer.
    */

  class TArrayBuffer[T](initialSize: Int = 8)
                       (implicit cm: scala.reflect.ClassTag[T]) extends scala.collection.mutable.Buffer[T] {
    private val underlying = Ref[Array[T]](Array.empty[T])
    override def apply(n: Int): T = atomic { implicit txn =>
      underlying().apply(n)
    }
    override def update(n: Int, newelem: T): Unit = atomic { implicit txn =>
      underlying().update(n, newelem)
      underlying()
    }
    override def length: Int = atomic { implicit txn =>
      underlying().length
    }
    override def +=(elem: T): TArrayBuffer.this.type = atomic { implicit txn =>
      underlying() = if (underlying().isEmpty) Array.apply(elem) else Array.concat(underlying(), Array.apply(elem))
      this
    }
    override def clear(): Unit = atomic { implicit txn =>
      underlying() = Array.empty[T]
    }
    override def +=:(elem: T): TArrayBuffer.this.type = atomic { implicit txn =>
      underlying() = Array.concat(Array.apply(elem), underlying())
      this
    }
    override def insertAll(n: Int, elems: Traversable[T]): Unit = atomic { implicit txn =>
      val (before, after) = underlying().splitAt(n)
      underlying() = Array.concat(before, elems.toArray, after)
    }
    override def remove(n: Int): T = atomic { implicit txn =>
      val rm = underlying().apply(n)
      underlying() = underlying().take(n) ++ underlying().drop(n+1)
      rm
    }
    override def iterator: Iterator[T] = atomic { implicit txn =>
      underlying().iterator
    }
  }

  /**
    * Класс TSortedList хранит данные в порядке сортировки, и для обращения
    * к последнему элементу требуется обойти все узлы списка, что может ухуд-
    * шить производительность программы. Для решения этой проблемы
    * можно использовать AVL-дерево (сбалансированное двоичное дерево поиска).
    * В Интернете можно найти массу описаний AVL-деревьев. Используйте ScalaSTM,
    * чтобы реализовать потокобезопасное, сортированное транзакционное множество как AVL-дерево:
    * class TSortedSet[T] {
    *   def add(x: T)(implicit txn: InTxn): Unit = ???
    *   def remove(x: T)(implicit txn: InTxn): Boolean = ???
    *   def apply(x: T)(implicit txn: InTxn): Boolean = ???
    * }
    * Класс TSortedSet имеет схожую семантику с scala.collection.mutable.Set .
    */

  class TSortedSet[T] {
    private val underlying = Ref[Set[T]](Set.empty[T])
    def add(x: T)(implicit txn: InTxn): Unit = atomic { implicit txn =>
      underlying() = underlying() + x
    }
    def remove(x: T)(implicit txn: InTxn): Boolean = atomic { implicit txn =>
      if (underlying().contains(x)) {
        underlying() = underlying() - x
        true
      } else {
        false
      }
    }
    def apply(x: T)(implicit txn: InTxn): Boolean = atomic { implicit txn =>
      if (underlying().contains(x)) {
        false
      } else {
        underlying() = underlying() + x
        true
      }
    }
  }

  /**
    * Используйте ScalaSTM для реализации банковской системы, отслеживающей суммы, хранящиеся на счетах клиентов.
    * Разные потоки могут вызывать
    * метод send для перевода денег со счета на счет,
    * метод deposit и
    * метод withdraw вносят или списывают суммы с конкретных счетов соответственно, а
    * метод totalStock возвращает общую сумму денег, в данный момент хранящихся на всех счетах в банке.
    * Наконец, реализуйте
    * метод totalStockIn , возвращающий общую сумму денег, в данный момент хранящихся на счетах заданной группы банков.
    */

  type UserId = Long
  type AccountId = Long
  type MoneyType = String
  type MoneyAmount = Int

  case class Account(id: AccountId, moneyType: MoneyType, funds: MoneyAmount)
  class Bank {
    import scala.collection.immutable.{Seq ⇒ GSeq}
    type Accounts = GSeq[Account]
    private val db = TMap[UserId, Accounts]()
    private val accountIdCounter = new AtomicLong()
    private val defaultType: MoneyType = "USD"


    def addUser(id: UserId): Option[Accounts] = atomic { implicit txn =>
      if (db.contains(id)) throw new RuntimeException("id exist")
      else {
        db.put(id, GSeq(Account(accountIdCounter.incrementAndGet(), defaultType, 0)))
        db.get(id)
      }
    }

    def getFunds(id: UserId): Accounts = db.single.get(id) match {
      case Some(funds) => funds
      case None => throw new RuntimeException("id not found")
    }

    def deposit(id: UserId, accountId: AccountId, amount: MoneyAmount): Unit = atomic { implicit txn =>
      db.get(id) match {
        case Some(accounts) =>
          val index = accounts.indexWhere(_.id == accountId)
          val acc = accounts(index)
          db.update(id, accounts.updated(index, acc.copy(funds = acc.funds + amount)))
        case None =>
          throw new RuntimeException("id not found")
      }
    }

    def withdraw(id: UserId, accountId: AccountId, amount: MoneyAmount): Unit = atomic { implicit txn =>
      db.get(id) match {
        case Some(accounts) => accounts.find(_.id == accountId) match {
          case Some(account) if account.funds >= amount ⇒
            val index = accounts.indexWhere(_.id == accountId)
            val acc = accounts(index)
            db.update(id, accounts.updated(index, acc.copy(funds = acc.funds - amount)))
          case Some(account) ⇒ throw new RuntimeException(s"amount $amount > funds ${account.funds}")
          case None ⇒ throw new RuntimeException("account id not found")
        }
        case None => throw new RuntimeException("id not found")
      }
    }

    def send(from: (UserId,AccountId), to: (UserId,AccountId), amount: MoneyAmount) = atomic { implicit txn =>
      val (fromUserId, fromAccountId) = from
      val (toUserId, toAccountId) = to

      (db.get(fromUserId), db.get(toUserId)) match {
        case (Some(fromAccounts), Some(toAccounts))
          if fromAccounts.exists(acc ⇒ acc.id == fromAccountId && acc.funds >= amount) && toAccounts.exists(_.id == toAccountId)  =>
          withdraw(fromUserId,fromAccountId,amount)
          deposit(toUserId, toAccountId, amount)
        case (None, _) => throw new RuntimeException("id not found")
        case (_, None) => throw new RuntimeException("id not found")
        case (Some(fromFunds), Some(toFunds)) => throw new RuntimeException(s"amount $amount > funds $fromFunds")
      }
    }
  }

}
