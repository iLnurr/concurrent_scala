package conc

import java.util.concurrent.ConcurrentHashMap

import scala.concurrent.{Await, Future, Promise}
import scala.io.Source
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.async.Async._
import scala.collection.concurrent
import scala.collection.concurrent.TrieMap
import scala.concurrent._
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

}
