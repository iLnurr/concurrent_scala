import scala.annotation.tailrec
@tailrec def recur(list: List[Int]): List[Int] = list match {
  case Nil => Nil
  case _ => {
    val negs = list.takeWhile(_ < 0)
    println(s"negs \n$negs")
    val other = list.dropWhile(_ < 0)
    println(s"other \n$other")
    require(negs.nonEmpty, "negs must be nonEmpty")
    require(other.nonEmpty && other.size >= negs.size, "poses must be ge that negs")
    val otherAfterPosDropFromLeft = other.dropWhile(_ > 0)
    println(s"otherAfterPosDropFromLeft \n$otherAfterPosDropFromLeft")
    val droppedFromLeftSize = other.size - otherAfterPosDropFromLeft.size
    println(s"droppedFromLeftSize \n$droppedFromLeftSize")
    //  require(otherAfterPosDropFromLeft.size >= droppedFromLeftSize, s"otherAfterPosDropFromLeft.size:${otherAfterPosDropFromLeft.size} must be >= droppedFromLeftSize:$droppedFromLeftSize")
    val needToDropFromRight = negs.size - droppedFromLeftSize
    val rightDropped = otherAfterPosDropFromLeft.takeRight(needToDropFromRight)
    require(rightDropped.size == needToDropFromRight && rightDropped.forall(_ > 0), "rightDropped.size == needToDropFromRight && rightDropped.forall(_ > 0)")
    val otherAfterPosDropFromRight = otherAfterPosDropFromLeft.dropRight(needToDropFromRight)
    println(s"otherAfterPosDropFromRight \n$otherAfterPosDropFromRight")
    val droppedFromRight = otherAfterPosDropFromLeft.size - otherAfterPosDropFromRight.size
    println(s"droppedFromRight \n$droppedFromRight")
    require(droppedFromLeftSize + droppedFromRight == negs.size, s"droppedFromLeftSize:$droppedFromLeftSize + droppedFromRight$droppedFromRight must be eq negs.size:${negs.size}")
    val rest = otherAfterPosDropFromRight
    println(s"rest \n$rest")
    recur(rest)
  }
}
// (()()())
val list = List(-1,-1,1,-1,1,-1,1,1)

//recur(list).isEmpty
def parallelBalanceParentheses(s: String): Boolean = {
  val result = s.aggregate(List.empty[Int])(
    (acc, char) => char match {
      case '(' => -1 :: acc
      case ')' => 1 :: acc
      case _ => acc
    },
    (acc1,acc2) => acc1 ++ acc2
  ).reverse

  println(result)

  result match {
    case Nil => true
    case List(_) => false
    case head :: tail if head == 1 || tail.last == -1 || result.size % 2 != 0 => false
    case unstable if unstable.sum != 0 => false
    case other =>
      recur(other).isEmpty
  }
}

parallelBalanceParentheses("0(1)(2(3))4")


