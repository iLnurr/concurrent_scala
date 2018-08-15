package ch2

import Accounts._

object AccountTest extends App {
  val range = (1 to 1000)
  val testAccounts = range.map(v => new Account(v.toString, v))
  val expectedResult = range.sum
  val targetAccount = new Account("target", 0)

  sendAll(testAccounts.toSet, targetAccount)

  require(targetAccount.money == expectedResult, s"transfer incorrect! ${targetAccount}")
  require(testAccounts.map(_.money).sum == 0, "all money from accounts must be transferred")

  println(expectedResult)

  println(s"${targetAccount.name} -> ${targetAccount.money}")
}
