package image_signature

import utest._

object SignatureCalcTest extends TestSuite {
  def tests = TestSuite {
    // TODO tests
    'getGridCoords {
      val coords = SignatureCalc.getGridCoords(10, 10, 10, 10)
      val expected = for (x <- 1 until 10; y <- 1 until 10) yield (x, y)
      assert(coords.length == 81)
      assert(coords == expected)
    }
    'normalizeValue {
      val normalizeWithCutoffs = SignatureCalc.normalizeValue(2, 5, -10)
      assert(normalizeWithCutoffs(1) == 0)
      assert(normalizeWithCutoffs(3) == 1)
      assert(normalizeWithCutoffs(8) == 2)
      assert(normalizeWithCutoffs(-7) == -1)
      assert(normalizeWithCutoffs(-11) == -2)
    }
  }
}
