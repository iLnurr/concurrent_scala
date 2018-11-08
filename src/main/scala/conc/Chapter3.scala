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
      private val underlying = List[AtomicReference[Option[T]]](new AtomicReference[Option[T]](None))
      private def add(x: T, list: List[AtomicReference[Option[T]]]): Unit = list match {
        case List(head) ⇒
          head.get() match {
            case None ⇒
              if (!head.compareAndSet(None, Some(x))) add(x, list)
            case Some(old) ⇒
              if (ord.compare(old, x) <= 0) {
                new AtomicReference[Option[T]](Some(x)) :: list
              } else {
                list ::: List(new AtomicReference[Option[T]](Some(x)))
              }
          }
        case h :: t ⇒
          h.get() match {
            case None ⇒
              if (!h.compareAndSet(None, Some(x))) add(x, list)
            case Some(old) ⇒
              if (ord.compare(old, x) <= 0) {
                new AtomicReference[Option[T]](Some(x)) :: list
              } else {
                add(x, t)
              }
          }

      }

      def add(x: T): Unit = add(x, underlying)

      def iterator: Iterator[T] = new Iterator[T] {
        private var current = underlying
        override def hasNext: Boolean = current.isEmpty

        override def next(): T = {
          current match {
            case Nil ⇒
              throw new NoSuchElementException("next on empty iterator")
            case List(single) ⇒
              current = Nil
              single.get() match {
                case None ⇒
                  throw new NoSuchElementException("next on empty iterator")
                case Some(el) ⇒
                  el
              }
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
