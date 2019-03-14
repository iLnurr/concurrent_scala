package conc

import Chapter5._
object Ch5Ex10 extends App {
  assert(parallelBalanceParentheses("0(1)(2(3))4"))
  assert(!parallelBalanceParentheses("0)2(1(3)"))
  assert(!parallelBalanceParentheses("0((1)2"))
}
