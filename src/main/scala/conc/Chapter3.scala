package conc

import java.util.concurrent.Executor
import java.util.concurrent.atomic.AtomicReference

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object Chapter3 {

  /**
    * Реализуйте собственный класс ExecutionContext с именем PiggybackContext,
    * выполняющий объекты Runnable в том же потоке, где вызывается метод
    * execute. Объект Runnable, выполняемый классом PiggybackContext, должен
    * иметь возможность сам вызывать метод execute, при этом все исключения
    * должны обрабатываться правильно.
    */
  object Ex1 {

    class PiggybackContext extends Executor {
      override def execute(command: Runnable): Unit = command.run()
    }

  }

  /**
    * Реализуйте класс TreiberStack – абстракцию конкурентного стека:
    * class TreiberStack[T] {
    *   def push(x: T): Unit = ???
    *   def pop(): T = ???
    * }
    * Используйте атомарную ссылку на связанный список узлов, прежде поме-
    * щенных в стек. Ваша реализация не должна блокировать вызывающий по-
    * ток и исключать вероятность проблемы ABA.
    */
  object Ex2 {
    class TreiberStack[T] {
      val parallelism = Runtime.getRuntime.availableProcessors() * 32
      val ar = new AtomicReference[List[T]](List.empty[T])
      def push(x: T): Unit = {
        @tailrec def retry(): Unit = {
          val v = ar.get()
          val nv = x :: v
          if (!ar.compareAndSet(v, nv)) retry()
        }
        retry()
      }
      def pop(): T = {
        @tailrec def retry(): T = {
          val old = ar.get()
          val nl = old.tail
          if (ar.compareAndSet(old, nl)) old.head else retry()
        }
        retry()
      }
    }
  }

  /**
    * Реализуйте класс ConcurrentSortedList – абстракцию конкурентного сортиро-
    * ванного списка:
    * class ConcurrentSortedList[T](implicit val ord: Ordering[T]) {
    * def add(x: T): Unit = ???
    * def iterator: Iterator[T] = ???
    * }
    * Внутренне класс ConcurrentSortedList должен использовать связанный спи-
    * сок атомарных ссылок. Ваша реализация не должна блокировать вызываю-
    * щий поток и исключать вероятность проблемы ABA.
    * Объект Iterator , возвращаемый методом iterator , должен правильно вы-
    * полнять обход элементов списка в порядке возрастания при условии отсут-
    * ствия конкурентных вызовов метода add .
    */
  object Ex3 {
    class ConcurrentSortedList[T](implicit val ord: Ordering[T]) {
      def empty = new AtomicReference[Option[T]](None)

      private val ar = new AtomicReference(List[AtomicReference[Option[T]]](empty))

      @tailrec final def addIn(x: T,
                acc: ListBuffer[AtomicReference[Option[T]]],
                list: List[AtomicReference[Option[T]]]): List[AtomicReference[Option[T]]] ={
        list match {
          case Nil ⇒
            val nv = empty
            nv.compareAndSet(None, Some(x))
            acc.toList ++ (nv :: list)
          case h :: t ⇒
            val hv = h.get()
            hv match {
              case None ⇒
                if (t.nonEmpty && t.head.get().nonEmpty && ord.compare(x, t.head.get().get) <= 0) {
                  if (!h.compareAndSet(hv, Some(x))) addIn(x, acc, list) else acc.toList ++ list
                } else {
                  addIn(x, acc += h, t)
                }
              case Some(old) ⇒
                if (ord.compare(x, old) <= 0) {
                  val nv = empty
                  nv.compareAndSet(None, Some(x))
                  acc.toList ++ (nv :: list)
                } else {
                  addIn(x, acc += h, t)
                }
            }
        }
      }

      def add(x: T): Unit = {
        val list = ar.get()
        val added = addIn(x, ListBuffer(), list)
        if (!ar.compareAndSet(list, added)) add(x)
      }

      def iterator: Iterator[T] = new Iterator[T] {
        private var current = ar.get()
        override def hasNext: Boolean = current.nonEmpty

        override def next(): T = {
          current match {
            case Nil ⇒
              throw new NoSuchElementException("next on empty iterator")
            case h :: t ⇒
              current = t
              h.get() match {
                case None ⇒
                  throw new NoSuchElementException("next on empty iterator")
                case Some(el) ⇒
                  el
              }
          }
        }
      }
    }
  }

  /**
    * Реализуйте класс LazyCell со следующим интерфейсом:
    * class LazyCell[T](initialization: =>T) { def apply(): T = ??? }
    * Создание объекта LazyCell и вызов метода apply должны иметь ту же семантику, как операции объявления значения с отложенной инициализацией и его чтения соответственно.
    * Вы не должны использовать в своей реализации значений с отложенной инициализацией.
    */
  object Ex4 {
    class LazyCell[T](initialization: => T) {
      @volatile private var isnt = Option.empty[T]
      private val lock = Array()
      def apply(): T = lock.synchronized(
        isnt match {
          case None ⇒
            val t = initialization
            isnt = Some(t)
            t
          case Some(already) ⇒
            already
        }
      )
    }
  }

  /**
    * Реализуйте класс PureLazyCell с тем же интерфейсом и семантикой, как класс LazyCell из предыдущего упражнения.
    * Класс PureLazyCell должен предполагать,
    * что выражение инициализации в параметре не имеет побочных эффектов и может выполняться неоднократно.
    * Метод apply не должен блокировать вызывающий поток и выполнять ини-
    * циализацию как можно меньшее количество раз.
    */
  object Ex5 {
    class PureLazyCell[T](initialization: => T) {
      private val ar = new AtomicReference[Option[T]](None)

      @tailrec
      final def apply(): T = ar.get match {
        case Some(v) => v
        case None => {
          val v = initialization
          if (!ar.compareAndSet(None, Some(v))) apply()
          else v
        }
      }
    }
  }
}
