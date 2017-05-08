package image_signature

import com.letstalkdata.scalinear.{ Matrix, Vector }
import utest._

object BorderedRectangleTest extends TestSuite {
  // [ 0 1 2 3 ]
  // [ 1 2 3 4 ]
  // [ 2 3 4 5 ]
  // [ 3 4 5 6 ]
  val rows1: IndexedSeq[Vector[Int]] = (0 to 3).map(n => Vector(n to n + 3:_*))
  val m1: Matrix[Int] = Matrix[Int](rows1:_*)

  // [ 0 1 2 3 4]
  // [ 1 2 3 4 5]
  // [ 2 3 4 5 6]
  // [ 3 4 5 6 7]
  // [ 4 5 6 7 8]
  val rows2: IndexedSeq[Vector[Int]] = (0 to 4).map(n => Vector(n to n + 4:_*))
  val m2: Matrix[Int] = Matrix[Int](rows2:_*)

  // [ 0 1 2 3 4 5 ]
  // [ 1 2 3 4 5 6 ]
  // [ 2 3 4 5 6 7 ]
  // [ 3 4 5 6 7 8 ]
  // [ 4 5 6 7 8 9 ]
  // [ 5 6 7 8 9 10]
  val rows3: IndexedSeq[Vector[Int]] = (0 to 5).map(n => Vector(n to n + 5:_*))
  val m3: Matrix[Int] = Matrix[Int](rows3:_*)

  def tests = TestSuite {
    'outerCornersNum {
      assert(BorderedRectangle.outerCornersNum(m1) == 4)
      assert(BorderedRectangle.outerCornersNum(m2) == 4)
      assert(BorderedRectangle.outerCornersNum(m3) == 4)
    }
    'outerCornersSum {
      assert(BorderedRectangle.outerCornersSum(m1) == 12)
      assert(BorderedRectangle.outerCornersSum(m2) == 16)
      assert(BorderedRectangle.outerCornersSum(m3) == 20)
    }
    'outerAdjacentToCornersNum {
      assert(BorderedRectangle.outerAdjacentToCornersNum(m1) == 8)
      assert(BorderedRectangle.outerAdjacentToCornersNum(m2) == 8)
      assert(BorderedRectangle.outerAdjacentToCornersNum(m3) == 8)
    }
    'outerAdjacentToCornersSum {
      assert(BorderedRectangle.outerAdjacentToCornersSum(m1) == 24)
      assert(BorderedRectangle.outerAdjacentToCornersSum(m2) == 32)
      assert(BorderedRectangle.outerAdjacentToCornersSum(m3) == 40)
    }
    'outerTopEdgeNum {
      assert(BorderedRectangle.outerTopEdgeNum(m1) == 0)
      assert(BorderedRectangle.outerTopEdgeNum(m2) == 1)
      assert(BorderedRectangle.outerTopEdgeNum(m3) == 2)
    }
    'outerTopEdgeSum {
      assert(BorderedRectangle.outerTopEdgeSum(m1) == 0)
      assert(BorderedRectangle.outerTopEdgeSum(m2) == 2)
      assert(BorderedRectangle.outerTopEdgeSum(m3) == 5)
    }
    'outerBottomEdgeNum {
      assert(BorderedRectangle.outerBottomEdgeNum(m1) == 0)
      assert(BorderedRectangle.outerBottomEdgeNum(m2) == 1)
      assert(BorderedRectangle.outerBottomEdgeNum(m3) == 2)
    }
    'outerBottomEdgeSum {
      assert(BorderedRectangle.outerBottomEdgeSum(m1) == 0)
      assert(BorderedRectangle.outerBottomEdgeSum(m2) == 6)
      assert(BorderedRectangle.outerBottomEdgeSum(m3) == 15)
    }
    'outerLeftEdgeNum {
      assert(BorderedRectangle.outerLeftEdgeNum(m1) == 0)
      assert(BorderedRectangle.outerLeftEdgeNum(m2) == 1)
      assert(BorderedRectangle.outerLeftEdgeNum(m3) == 2)
    }
    'outerLeftEdgeSum {
      assert(BorderedRectangle.outerLeftEdgeSum(m1) == 0)
      assert(BorderedRectangle.outerLeftEdgeSum(m2) == 2)
      assert(BorderedRectangle.outerLeftEdgeSum(m3) == 5)
    }
    'outerRightEdgeNum {
      assert(BorderedRectangle.outerRightEdgeNum(m1) == 0)
      assert(BorderedRectangle.outerRightEdgeNum(m2) == 1)
      assert(BorderedRectangle.outerRightEdgeNum(m3) == 2)
    }
    'outerRightEdgeSum {
      assert(BorderedRectangle.outerRightEdgeSum(m1) == 0)
      assert(BorderedRectangle.outerRightEdgeSum(m2) == 6)
      assert(BorderedRectangle.outerRightEdgeSum(m3) == 15)
    }
    'outerEdgesNum {
      assert(BorderedRectangle.outerEdgesNum(m1) == 0)
      assert(BorderedRectangle.outerEdgesNum(m2) == 4)
      assert(BorderedRectangle.outerEdgesNum(m3) == 8)
    }
    'outerEdgesSum {
      assert(BorderedRectangle.outerEdgesSum(m1) == 0)
      assert(BorderedRectangle.outerEdgesSum(m2) == 16)
      assert(BorderedRectangle.outerEdgesSum(m3) == 40)
    }
    'innerCornersNum {
      assert(BorderedRectangle.innerCornersNum(m1) == 4)
      assert(BorderedRectangle.innerCornersNum(m2) == 4)
      assert(BorderedRectangle.innerCornersNum(m3) == 4)
    }
    'innerCornersSum {
      assert(BorderedRectangle.innerCornersSum(m1) == 12)
      assert(BorderedRectangle.innerCornersSum(m2) == 16)
      assert(BorderedRectangle.innerCornersSum(m3) == 20)
    }
    'innerTopEdgeNum {
      assert(BorderedRectangle.innerTopEdgeNum(m1) == 0)
      assert(BorderedRectangle.innerTopEdgeNum(m2) == 1)
      assert(BorderedRectangle.innerTopEdgeNum(m3) == 2)
    }
    'innerTopEdgeSum {
      assert(BorderedRectangle.innerTopEdgeSum(m1) == 0)
      assert(BorderedRectangle.innerTopEdgeSum(m2) == 3)
      assert(BorderedRectangle.innerTopEdgeSum(m3) == 7)
    }
    'innerBottomEdgeNum {
      assert(BorderedRectangle.innerBottomEdgeNum(m1) == 0)
      assert(BorderedRectangle.innerBottomEdgeNum(m2) == 1)
      assert(BorderedRectangle.innerBottomEdgeNum(m3) == 2)
    }
    'innerBottomEdgeSum {
      assert(BorderedRectangle.innerBottomEdgeSum(m1) == 0)
      assert(BorderedRectangle.innerBottomEdgeSum(m2) == 5)
      assert(BorderedRectangle.innerBottomEdgeSum(m3) == 13)
    }
    'innerLeftEdgeNum {
      assert(BorderedRectangle.innerLeftEdgeNum(m1) == 0)
      assert(BorderedRectangle.innerLeftEdgeNum(m2) == 1)
      assert(BorderedRectangle.innerLeftEdgeNum(m3) == 2)
    }
    'innerLeftEdgeSum {
      assert(BorderedRectangle.innerLeftEdgeSum(m1) == 0)
      assert(BorderedRectangle.innerLeftEdgeSum(m2) == 3)
      assert(BorderedRectangle.innerLeftEdgeSum(m3) == 7)
    }
    'innerRightEdgeNum {
      assert(BorderedRectangle.innerRightEdgeNum(m1) == 0)
      assert(BorderedRectangle.innerRightEdgeNum(m2) == 1)
      assert(BorderedRectangle.innerRightEdgeNum(m3) == 2)
    }
    'innerRightEdgeSum {
      assert(BorderedRectangle.innerRightEdgeSum(m1) == 0)
      assert(BorderedRectangle.innerRightEdgeSum(m2) == 5)
      assert(BorderedRectangle.innerRightEdgeSum(m3) == 13)
    }
    'innerEdgesNum {
      assert(BorderedRectangle.innerEdgesNum(m1) == 0)
      assert(BorderedRectangle.innerEdgesNum(m2) == 4)
      assert(BorderedRectangle.innerEdgesNum(m3) == 8)
    }
    'innerEdgesSum {
      assert(BorderedRectangle.innerEdgesSum(m1) == 0)
      assert(BorderedRectangle.innerEdgesSum(m2) == 16)
      assert(BorderedRectangle.innerEdgesSum(m3) == 40)
    }
    'innerNum {
      assert(BorderedRectangle.innerNum(m1) == 0)
      assert(BorderedRectangle.innerNum(m2) == 1)
      assert(BorderedRectangle.innerNum(m3) == 4)
    }
    'innerSum {
      assert(BorderedRectangle.innerSum(m1) == 0)
      assert(BorderedRectangle.innerSum(m2) == 4)
      assert(BorderedRectangle.innerSum(m3) == 20)
    }
  }
}
