package conc

import io.reactors._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration._

object Chapter10 {
  /**
    * Определите метод twice, принимающий целевой канал и возвращающий другой канал,
    * который дважды посылает каждое событие в целевой канал.
    * def twice[T](target: Channel[T]): Channel[T]
    */
  def twice[T: Arrayable](target: Channel[T])
                         (implicit system: ReactorSystem): Channel[T] = {
    system.spawn(Reactor[T] { self ⇒
      self.main.events onEvent { t ⇒
        println(s"twice proxy channel catch event= `$t`")
        target ! t
        target ! t
      }
    })
  }

  /**
    * Определите метод throttle, снижающий пропускную способность целевого
    * канала до определенного числа событий в единицу времени.
    * def throttle[T](target: Channel[T]): Channel[T]
    * Подсказка: используйте службу Clock и прием композиции потоков событий.
    */

  def throttle[T: Arrayable](target: Channel[T], duration: Duration = 2.seconds)
                            (implicit system: ReactorSystem): Channel[T] = {
    system.spawn(Reactor[T] { self ⇒
      val buffer = ArrayBuffer[T]()
      self.main.events onEvent { t ⇒
        buffer.append(t)
      }
      system.clock.periodic(duration) on {
        println("throttle")
        buffer.headOption.foreach{ t ⇒
          target ! t
          buffer.remove(0)
        }
      }
    })
  }

  /**
    * Реализуйте ненадежный широковещательный протокол, доставляющий события нескольким адресатам.
    * Метод broadcast должен реализовать следующий интерфейс,
    * где возвращаемый канал должен пересылать получаемые события во все целевые каналы:
    * def broadcast(targets: Seq[Channel[T]]): Channel[T]
    */
  def broadcast[T: Arrayable](targets: Seq[Channel[T]])
                             (implicit system: ReactorSystem): Channel[T] = {
    system.spawn(Reactor[T] { self ⇒
      self.main.events onEvent { t ⇒
        targets.foreach(_ ! t)
      }
    })
  }

  /**
    * Узнайте, как действует алгоритм счетчика CRDT (Convergent Replicated Data Types – Бесконфликтные репликативные структуры данных).
    * Затем используйте ненадежный широковещательный протокол из предыдущего упражнения для его реализации.
    * Определите метод crdt, позволяющий пользователям создавать счетчики CRDT.
    */

  class GCrdtReactorStateBased(id: Long)
                              (implicit system: ReactorSystem) {
    private val targets = ArrayBuffer[Channel[GCrdtReactorStateBased]]()
    val channel: Channel[GCrdtReactorStateBased] = system.spawn(Reactor[GCrdtReactorStateBased] { self ⇒
      self.main.events onEvent { t ⇒
        println(s"receive event ${t.counterSnap}")
        merge(t)
      }
    })
    private val counter = mutable.Map[Long,Long](id → 0)
    def counterSnap: Map[Long, Long] = counter.toMap
    def broadcastTo(crdts: GCrdtReactorStateBased*): Unit = targets.appendAll(crdts.map(_.channel))
    def increment(x: Long): Unit = {
      counter(id) = counter(id) + x
      targets.foreach(_ ! this)
    }
    def merge(other: GCrdtReactorStateBased): Unit = {
      val otherMap = other.counterSnap
      for {
        (k,v) ← otherMap
      } yield {
        val old = counter.getOrElse(k,0L)
        val newV = math.max(old,v)
        counter.update(k,newV)
      }
    }
    def value: Long = counter.valuesIterator.sum
  }

  import protocol._
  /**
    * Реализуйте надежный широковещательный протокол, имеющий тот же интерфейс,
    * что и ненадежный широковещательный протокол,
    * но гарантирующий доставку всем или ни одному адресату,
    * даже если отправитель аварийно завершился в процессе выполнения операции отправки.
    * Реализуйте модульные тесты для проверки вашей реализации.
    */
  def broadcastReliable[T: Arrayable](targets: Seq[Channel[T]])
                             (implicit system: ReactorSystem): Channel[T] = {
    var msgs: Map[Channel[T], ArrayBuffer[T]] = targets.map(ch ⇒ ch → ArrayBuffer.empty[T]).toMap
    system.spawn(Reactor[T] { self ⇒
      val (confirmChannel, confirmEvents) = mkConnector[T, Channel[T]](self)

      val injected = targets.map(ch ⇒ ch.inject{ t =>
        confirmChannel ! (ch,t)
      })
      self.main.events onEvent { t ⇒
        msgs = msgs.mapValues(seq ⇒ seq :+ t)
        injected.foreach(_ ! t)
      }
      confirmEvents.onEvent { case (ch,t) ⇒
        for {
          buf ← msgs.get(ch)
          if buf.indexOf(t) != -1
        } yield {
          buf.remove(buf.indexOf(t))
          msgs = msgs.updated(ch, buf)
        }
      }
    })
  }

  private def mkConnector[T,R: Arrayable](r: Reactor[T]) = {
    val c = r.system.channels.open[(R,T)]
    c.channel → c.events
  }
}
