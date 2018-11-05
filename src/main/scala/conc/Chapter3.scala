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
}
