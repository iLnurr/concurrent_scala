package conc

import conc.Chapter7.Bank

object Ch7Ex8Test extends App {
  val bank = new Bank
  val range = 1 to 1000
  val testAccounts = range.map{ i ⇒
    val account = bank.addUser(i).get.head
    bank.deposit(i, account.id, i)
    i → bank.getFunds(i).head
  }
  val expectedResult = range.sum
  val targetAccount = bank.addUser(0).get.head

  testAccounts.foreach{ case (userId,account) ⇒
    bank.send((userId, account.id), (0, targetAccount.id), account.funds)
  }

  val resultTargetAccount = bank.getFunds(0).head
  require(resultTargetAccount.funds == expectedResult, s"transfer incorrect! ${resultTargetAccount}")
  require(range.map(id ⇒ bank.getFunds(id).head.funds).sum == 0, "all money from accounts must be transferred")
}
