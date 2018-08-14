package ch2

import ch2._
import Ch2._
import Accounts._

object AccountTest extends App {
  val range = (1 to 1000)
  val testAccounts = range.map(v => new Account(v.toString, v))
  val expectedResult = range.sum
  val targetAccount = new Account("target", 0)

  sendAll(testAccounts.toSet, targetAccount)

  require(targetAccount.money == expectedResult, s"transfer incorrect! ${targetAccount}")

  println(expectedResult)

  println(s"${targetAccount.name} -> ${targetAccount.money}")
}
