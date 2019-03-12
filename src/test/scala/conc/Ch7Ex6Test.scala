package conc

import Chapter7._

object Ch7Ex6Test extends App {
  // TArrayBuffer tests.
  {
    // for `+=`.
    {
      val buf = new TArrayBuffer[Int](1)
      buf += 1
      buf += 2
      buf += 3
      assert(buf(0) == 1, buf)
      assert(buf(1) == 2)
      assert(buf(2) == 3)
      assert(buf.length == 3)
    }

    // for `+=:`
    {
      val buf = new TArrayBuffer[Int](1)
      3 +=: buf
      2 +=: buf
      1 +=: buf
      assert(buf(0) == 1)
      assert(buf(1) == 2)
      assert(buf(2) == 3)
      assert(buf.length == 3)
    }

    // for `insertAll`.
    {
      // insert into the index 0.
      val buf1 = new TArrayBuffer[Int](2)
      buf1 += 2
      buf1 += 3
      buf1.insertAll(0, List(1))
      assert(buf1(0) == 1)
      assert(buf1(1) == 2)
      assert(buf1(2) == 3)
      assert(buf1.length == 3)

      // insert into the index `length`.
      val buf2 = new TArrayBuffer[Int](2)
      buf2 += 1
      buf2.insertAll(1, List(2, 3))
      assert(buf2(0) == 1)
      assert(buf2(1) == 2)
      assert(buf2(2) == 3)
      assert(buf2.length == 3)

      // insert into any index (0 < n < `length`).
      val buf3 = new TArrayBuffer[Int](2)
      buf3 += 1
      buf3 += 4
      buf3.insertAll(1, List(2, 3))
      assert(buf3(0) == 1)
      assert(buf3(1) == 2)
      assert(buf3(2) == 3)
      assert(buf3(3) == 4)
      assert(buf3.length == 4)
    }

    // for `remove`.
    {
      val buf = new TArrayBuffer[Int]()
      buf += 1
      buf += 2
      buf += 3
      val removed = buf.remove(1)
      assert(removed == 2)
      assert(buf(0) == 1, buf)
      assert(buf(1) == 3)
      assert(buf.length == 2)
    }

    // for `update`.
    {
      val buf = new TArrayBuffer[Int]()
      buf += 1
      buf += 2
      buf(0) = 3
      assert(buf(0) == 3)
      assert(buf(1) == 2)
    }

    // for `clear`.
    {
      val buf = new TArrayBuffer[Int]()
      buf += 1
      buf.clear()
      assert(buf.length == 0)
    }

    // for `iterator`.
    {
      val buf = new TArrayBuffer[Int]()
      buf += 1
      buf += 2
      assert(buf.toList == List(1, 2))
    }

    // for concurrency of `+=`.
    {
      val buf = new TArrayBuffer[Int](1)
      val threads = (1 to 10).map(i => new Thread {
        override def run(): Unit = {
          Thread.sleep(15) // tweak not to append `i` in iteration order.
          buf += i
        }
      })
      threads.foreach(_.start())
      threads.foreach(_.join())
      assert(buf.length == 10)
    }

    // for concurrency of `+=:`.
    {
      val buf = new TArrayBuffer[Int](1)
      val threads = (1 to 10).map(i => new Thread {
        override def run(): Unit = {
          Thread.sleep(15)
          i +=: buf
        }
      })
      threads.foreach(_.start())
      threads.foreach(_.join())
      assert(buf.length == 10)
    }

    // for concurrency of `insertAll`.
    {
      val buf = new TArrayBuffer[Int](1)
      val threads = (1 to 10).map(i => new Thread {
        override def run(): Unit = {
          Thread.sleep(15)
          buf.insertAll(0, List(i, i * 10))
        }
      })
      threads.foreach(_.start())
      threads.foreach(_.join())
      assert(buf.length == 20)
      (0 until 20 by 2).foreach(i => {
        assert(buf(i + 1) == buf(i) * 10, s"${buf(i)}, ${buf(i + 1)}")
      })
    }

    // for concurrency of `remove`.
    {
      val buf = new TArrayBuffer[Int]()
      buf.insertAll(0, 1 to 10)
      val threads = (1 to 10).map(i => new Thread {
        override def run(): Unit = {
          Thread.sleep(15)
          buf.remove(0)
        }
      })
      threads.foreach(_.start())
      threads.foreach(_.join())
      assert(buf.length == 0)
    }
  }
}
