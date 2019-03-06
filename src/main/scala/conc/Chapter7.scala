package conc

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
    def swap()(implicit e: P =:= Q, txn: InTxn): Unit = atomic { implicit txn =>
      val p = refP.get
      val q = refQ.get
      refP.swap(q.asInstanceOf[P])
      refQ.swap(p.asInstanceOf[Q])
    }
  }

}
