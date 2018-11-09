package conc

import java.util.concurrent.Executor
import java.util.concurrent.atomic.AtomicReference

import scala.annotation.tailrec

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

      def add(x: T): Unit = {
        def addIn(x: T, list: List[AtomicReference[Option[T]]]): List[AtomicReference[Option[T]]] ={
          list match {
            case Nil ⇒
              throw new IllegalStateException("Underlying list must be non empty")
            case h :: t ⇒
              val hv = h.get()
              hv match {
                case None ⇒
                  val isLess = t.headOption.flatMap(_.get().map(ord.compare(x, _) <= 0)).getOrElse(true)

                  if (isLess) {
                    if (!h.compareAndSet(hv, Some(x))) addIn(x, list) else list
                  } else {
                    if (t.isEmpty) h :: addIn(x, empty :: t) else h :: addIn(x, t)
                  }
                case Some(old) ⇒
                  if (ord.compare(x, old) <= 0) {
                    addIn(x, empty :: list)
                  } else {
                    if (t.isEmpty) h :: addIn(x, empty :: t) else h :: addIn(x, t)
                  }
              }
          }
        }
        val list = ar.get()
        val added = addIn(x, list)
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
}
