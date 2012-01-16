import scala.math.{UInt, ULong}

object Test extends App {

  def testLongDivMod() {
    val mask: BigInt = BigInt("FFFFFFFFFFFFFFFF", 16)

    def udivmod2 (n: Long, d: Long) = {
      var nl: BigInt = BigInt(n) & mask
      var dl: BigInt = BigInt(d) & mask
      ((nl / dl).toLong, (nl % dl).toLong)
    }

    def test(i1: Long, i2: Long): Unit = {
      import ULong._
      val r1 = udivmod(i1, i2)
      val r2 = udivmod2(i1, i2)
      assert(r1 == r2, i1+" /% "+i2+" = "+r1+", should be "+r2)
    }

    import scala.util.Random

    val N = 20000000

    for (i <- 0 until N) test(Random.nextLong(), Random.nextLong())

    def testX(n: Long) = {
      test(n, 1)
      test(n, -1)
      test(n, 2)
      test(n, -2)
      test(n, Long.MaxValue)
      test(n, Long.MinValue)
    }

    testX(0)
    testX(1)
    testX(-1)
    testX(2)
    testX(-2)
    testX(Long.MaxValue)
    testX(Long.MinValue)

    println(N+" tests passed")
  }

  testLongDivMod()
  
}
