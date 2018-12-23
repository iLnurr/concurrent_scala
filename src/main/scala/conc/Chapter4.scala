package conc

import java.util.concurrent.ConcurrentHashMap

import scala.annotation.tailrec
import scala.concurrent.{Await, Future, Promise}
import scala.io.Source
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.async.Async._
import scala.collection.concurrent
import scala.collection.concurrent.TrieMap
import scala.concurrent._
import scala.util.{Failure, Success}
import scala.util.control.NonFatal

object Chapter4 {
  def delay(n: Int): Future[Unit] = async { blocking { Thread.sleep(n * 1000) } }
  /**
    * Реализуйте программу командной строки,
    * предлагающую пользователю ввести URL некоторого веб-сайта и
    * отображающую разметку HTML этого веб-сайта.
    * Между моментом, когда пользователь нажмет клавишу ENTER и когда будет получена HTML-страница,
    * программа должна каждые 50 миллисекунд выводить символ точки.
    * При этом предельное время ожидания получения страницы составляет 2 секунды.
    * Используйте только объекты Future и Promise, и избегайте примитивов синхронизации, описанных в предыдущей главе.
    * Вы можете повторно использовать метод timeout из этой главы.
    */
  object Ex1 {
    def recurUrl(): String = try {
      println("Please type url and hit enter")
      val line = scala.io.StdIn.readLine()
      val url = RefinedUrl.unsafeFrom(line)
      url.value
    } catch {
      case _: IllegalArgumentException ⇒
        recurUrl()
    }

    def run() = {
      val urlS = recurUrl()
      val future = Future(Source.fromURL(urlS).mkString)
      val timer = mytimer(println("."),1,10)
      val html = Await.result(future, 2.seconds)
      timer.cancel()
      println(s"html:\n $html")

    }
  }


  /**
    * Реализуйте абстракцию переменной однократного присваивания в виде класса IVar:
      class IVar[T] {
        def apply(): T = ???
        def :=(x: T): Unit = ???
      }
    * Сразу после создания экземпляр IVar не должен содержать значения и вызов apply должен приводить к исключению.
    * После присваивания значения с помощью метода := последующие вызовы := должны возбуждать исключения,
    * а вызов метода apply должен возвращать прежде присвоенное значение.
    * Используйте только объекты Future и Promise и избегайте примитивов синхронизации, описанных в предыдущей главе.
    */
  object Ex2 {
    class IVar[T] {
      private val p = Promise[T]
      def apply(): T = if (p.isCompleted) {
        Await.result(p.future, 2.seconds)
      } else {
        Await.result(
          p.failure(new IllegalStateException("Already completed")).future,
          2.seconds
        )
      }
      def :=(x: T): Unit = if (!p.trySuccess(x)) p.failure(new IllegalStateException("Already completed"))
    }
  }

  /**
    * Добавьте в тип Future[T] метод exists, принимающий предикат и возвращающий объект Future[Boolean]:
    * def exists(p: T => Boolean): Future[Boolean]
    * Возвращаемый объект Future завершится со значением true,
    * только если завершится оригинальный объект Future и предикат вернет true,
    * иначе – со значением false.
    * Вы можете использовать комбинаторы, но не должны создавать никаких объектов Promise.
    */
  object Ex3 {
    implicit class FutureExistsPredOps[T](val f: Future[T]) extends AnyVal {
      def exists(p: T => Boolean): Future[Boolean] = f.map(p).recover{case NonFatal(_) ⇒ false }
    }
  }

  /**
    * Повторите предыдущее упражнение, но вместо комбинаторов используйте объекты Promise.
    */
  object Ex4 {
    implicit class FutureExistsPredOps[T](val f: Future[T]) extends AnyVal {
      def exists(p: T => Boolean): Future[Boolean] = {
        val promise = Promise[Boolean]
        f.foreach(t ⇒ promise.success(p(t)))
        f.failed.foreach(_ ⇒ promise.success(false))
        promise.future
      }
    }
  }

  /**
    * Повторите предыдущее упражнение с использованием фреймворка Scala Async.
    */
  object Ex5 {
    implicit class FutureExistsPredOps[T](val f: Future[T]) {
      def exists(p: T => Boolean): Future[Boolean] =
        async {
          val v = await(f)
          p(v)
        } recover {
          case _ => false
        }
    }
  }

  /**
    * Реализуйте класс IMap, представляющий словарь с поддержкой однократного присваивания:
    class IMap[K, V] {
      def update(k: K, v: V): Unit
      def apply(k: K): Future[V]
    }
    * Пары ключ/значение могут добавляться в объект IMap, но никогда – удаляться или изменяться.
    * Каждый конкретный ключ можно добавить в словарь только один раз,
    * и все последующие вызовы update с этим же ключом должны возбуждать исключение.
    * Вызов apply с конкретным ключом должен возвращать объект Future,
    * который завершится после добавления этого ключа в словарь.
    * Помимо объектов Future и Promise в реализации можно использовать класс scala.collection.concurrent.Map.
    */
  object Ex7 {
    class AlreadyAssocKeyException(msg: String) extends RuntimeException(msg)
    trait IMap[K, V] {
      val underlying: scala.collection.concurrent.Map[K,Promise[V]]
      def update(k: K, v: V): Unit = {
        underlying.get(k) match {
          case Some(old) if old.isCompleted ⇒
            throw new AlreadyAssocKeyException(s"Key $k already assoc")
          case Some(nonCompletedPromise) ⇒
            val pr = nonCompletedPromise.success(v)
            underlying.put(k, pr)
          case None ⇒
            underlying.put(k, Promise.successful(v))
        }
      }
      def apply(k: K): Future[V] = {
        underlying.get(k) match {
          case Some(value) ⇒
            value.future
          case None ⇒
            val pr = Promise[V]
            underlying.put(k, pr)
            pr.future
        }
      }
    }

    class IMapImpl[K,V] extends IMap[K,V] {
      import scala.collection.convert.decorateAsScala._
      override val underlying: concurrent.Map[K, Promise[V]] = new ConcurrentHashMap[K, Promise[V]]().asScala
    }
  }

  /**
    * Добавьте в тип Promise[T] метод compose, принимающий функцию типа S => T и возвращающий объект Promise[S]:
      def compose[S](f: S => T): Promise[S]
    * Всякий раз, когда возвращаемый объект Promise завершается с некоторым значением x типа S (или с ошибкой),
    * оригинальный объект Promise должен асинхронно завершиться со значением f(x) (или с ошибкой),
    * если оригинальный объект Promise еще не завершился.
    */
  object Ex8 {
    implicit class PromiseComposeOps[T](val pt: Promise[T]) extends AnyVal {
      def compose[S](f: S => T): Promise[S] = {
        val ps = Promise[S]

        ps.future.onComplete{
          case Success(s) ⇒ pt.trySuccess(f(s))
          case Failure(e) ⇒ pt.tryFailure(e)
        }

        ps
      }
    }
  }

  /**
    * Реализуйте метод scatterGather, принимающий последовательность заданий,
    * выполняющий их параллельно как асинхронные вычисления,
    * объединяющий результаты и возвращающий объект Future,
    * который содержит последовательность результатов разных заданий.
    * Метод scatterGather должен иметь следующий интерфейс:
      def scatterGather[T](tasks: Seq[() => T]): Future[Seq[T]]
    */
  object Ex9 {
    def scatterGather[T](tasks: Seq[() => T]): Future[Seq[T]] = {
      Future.sequence(for {
        task ← tasks
      } yield {
        Future(
          task()
        )
      })
    }
  }

  /**
    * Реализуйте другую версию метода timeout, представленного в этой главе,
    * но без использования конструкции blocking или Thread.sleep.
    * Используйте класс java.util.Timer из JDK.
    * Какими преимуществами обладает новая реализация?
    */
  object Ex10 {
    import java.util._
    private val timer = new Timer(true)
    def timeout(t: Long): Future[Unit] = {
      val p = Promise[Unit]
      timer.schedule(
        new TimerTask {
          def run() = {
            p success ()
            timer.cancel() }
        }, t)
      p.future
    }
  }

  /**
    * Ориентированный граф – это структура данных, состоящая из конечно- го количества узлов,
    * каждый из которых имеет конечное количество направленных ребер, соединяющих его с другими узлами.
    * Ориентированный ациклический граф – это ориентированный граф, в котором любой путь из любого узла N
    * в любой другой узел по направленным ребрам не приведет обратно в узел N.
    * Иными словами, ориентированные ребра в ориентированном ациклическом графе никогда не образуют замкнутых циклов.
    * Вот как можно было бы представить узлы в ориентированном ациклическом графе:
    class DAG[T](val value: T) {
      val edges = scala.collection.mutable.Set[DAG[T]]
    }
    * Вот пример объявления графа:
    val a = new DAG("a")
    val b = new DAG("b")
    val c = new DAG("c")
    val d = new DAG("d")
    val e = new DAG("e")

    a.edges += b
    b.edges += c
    b.edges += d
    c.edges += e
    d.edges += e
    *
    * Ориентированные ациклические графы часто используются для описания зависимостей между разными элементами,
    * например заданий для инструментов сборки в проекте или в IDE.
    * Вы должны реализовать метод fold, принимающий узел графа и функцию,
    * отображающую каждый элемент и его входные данные в некоторое значение,
    * который возвращает метод Future со значением узла:
      def fold[T, S](g: DAG[T], f: (T, Seq[S]) => S): Future[S]
    * Метод fold должен запустить асинхронное задание для каждого узла в графе,
    * чтобы отобразить элемент и его входные данные в новое значение.
    * Зависимости между элементами обязательно должны учитываться: элемент может запускаться только после выполнения
    * всех его зависимостей. Например, задание b можно запустить только после того, как задания c и d вернут результат.
    */
  object Ex11 {
    class DAG[T](val value: T) {
      val edges = scala.collection.mutable.Set[DAG[T]]()
    }

    def fold[T, S](g: DAG[T], f: (T, Seq[S]) => S): Future[S] = {
//      @tailrec
      def recur(gg: DAG[T]): S = gg.edges match {
        case empty if empty.isEmpty ⇒
          f(gg.value, Nil)
        case nonEmpty ⇒
          f(
            gg.value,
            nonEmpty.map(recur).toSeq
          )
      }

      Future(recur(g))
    }
  }

}
