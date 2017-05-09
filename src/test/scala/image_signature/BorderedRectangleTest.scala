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

    'outerCorners {
      val r1: Array[Int] = BorderedRectangle.outerCorners(m1)
      val r2: Array[Int] = BorderedRectangle.outerCorners(m2)
      val r3: Array[Int] = BorderedRectangle.outerCorners(m3)

      assert(r1.sameElements(Array(0, 3, 3, 6)))
      assert(r2.sameElements(Array(0, 4, 4, 8)))
      assert(r3.sameElements(Array(0, 5, 5, 10)))
    }
    'outerAdjacentToCorners {
      val r1: Array[Int] = BorderedRectangle.outerAdjacentToCorners(m1)
      val r2: Array[Int] = BorderedRectangle.outerAdjacentToCorners(m2)
      val r3: Array[Int] = BorderedRectangle.outerAdjacentToCorners(m3)

      assert(r1.sameElements(Array(1, 2, 1, 4, 2, 5, 4, 5)))
      assert(r2.sameElements(Array(1, 3, 1, 5, 3, 7, 5, 7)))
      assert(r3.sameElements(Array(1, 4, 1, 6, 4, 9, 6, 9)))
    }
    'outerTopEdge {
      val r1: Array[Int] = BorderedRectangle.outerTopEdge(m1)
      val r2: Array[Int] = BorderedRectangle.outerTopEdge(m2)
      val r3: Array[Int] = BorderedRectangle.outerTopEdge(m3)

      assert(r1.length == 0)
      assert(r2.sameElements(Array(2)))
      assert(r3.sameElements(Array(2, 3)))
    }
    'outerBottomEdge {
      val r1: Array[Int] = BorderedRectangle.outerBottomEdge(m1)
      val r2: Array[Int] = BorderedRectangle.outerBottomEdge(m2)
      val r3: Array[Int] = BorderedRectangle.outerBottomEdge(m3)

      assert(r1.length == 0)
      assert(r2.sameElements(Array(6)))
      assert(r3.sameElements(Array(7, 8)))
    }
    'outerLeftEdge {
      val r1: Array[Int] = BorderedRectangle.outerLeftEdge(m1)
      val r2: Array[Int] = BorderedRectangle.outerLeftEdge(m2)
      val r3: Array[Int] = BorderedRectangle.outerLeftEdge(m3)

      assert(r1.length == 0)
      assert(r2.sameElements(Array(2)))
      assert(r3.sameElements(Array(2, 3)))
    }
    'outerRightEdge {
      val r1: Array[Int] = BorderedRectangle.outerRightEdge(m1)
      val r2: Array[Int] = BorderedRectangle.outerRightEdge(m2)
      val r3: Array[Int] = BorderedRectangle.outerRightEdge(m3)

      assert(r1.length == 0)
      assert(r2.sameElements(Array(6)))
      assert(r3.sameElements(Array(7, 8)))
    }
    'outerEdges {
      val r1: Array[Int] = BorderedRectangle.outerEdges(m1)
      val r2: Array[Int] = BorderedRectangle.outerEdges(m2)
      val r3: Array[Int] = BorderedRectangle.outerEdges(m3)

      assert(r1.length == 0)
      assert(r2.sameElements(Array(2, 2, 6, 6)))
      assert(r3.sameElements(Array(2, 3, 2, 3, 7, 8, 7, 8)))
    }
    'innerCorners {
      val r1: Array[Int] = BorderedRectangle.innerCorners(m1)
      val r2: Array[Int] = BorderedRectangle.innerCorners(m2)
      val r3: Array[Int] = BorderedRectangle.innerCorners(m3)

      assert(r1.sameElements(Array(2, 3, 3, 4)))
      assert(r2.sameElements(Array(2, 4, 4, 6)))
      assert(r3.sameElements(Array(2, 5, 5, 8)))
    }
    'innerTopEdge {
      val r1: Array[Int] = BorderedRectangle.innerTopEdge(m1)
      val r2: Array[Int] = BorderedRectangle.innerTopEdge(m2)
      val r3: Array[Int] = BorderedRectangle.innerTopEdge(m3)

      assert(r1.length == 0)
      assert(r2.sameElements(Array(3)))
      assert(r3.sameElements(Array(3, 4)))
    }
    'innerBottomEdge {
      val r1: Array[Int] = BorderedRectangle.innerBottomEdge(m1)
      val r2: Array[Int] = BorderedRectangle.innerBottomEdge(m2)
      val r3: Array[Int] = BorderedRectangle.innerBottomEdge(m3)

      assert(r1.length == 0)
      assert(r2.sameElements(Array(5)))
      assert(r3.sameElements(Array(6, 7)))
    }
    'innerLeftEdge {
      val r1: Array[Int] = BorderedRectangle.innerLeftEdge(m1)
      val r2: Array[Int] = BorderedRectangle.innerLeftEdge(m2)
      val r3: Array[Int] = BorderedRectangle.innerLeftEdge(m3)

      assert(r1.length == 0)
      assert(r2.sameElements(Array(3)))
      assert(r3.sameElements(Array(3, 4)))
    }
    'innerRightEdge {
      val r1: Array[Int] = BorderedRectangle.innerRightEdge(m1)
      val r2: Array[Int] = BorderedRectangle.innerRightEdge(m2)
      val r3: Array[Int] = BorderedRectangle.innerRightEdge(m3)

      assert(r1.length == 0)
      assert(r2.sameElements(Array(5)))
      assert(r3.sameElements(Array(6, 7)))
    }
    'innerEdges {
      val r1: Array[Int] = BorderedRectangle.innerEdges(m1)
      val r2: Array[Int] = BorderedRectangle.innerEdges(m2)
      val r3: Array[Int] = BorderedRectangle.innerEdges(m3)

      assert(r1.length == 0)
      assert(r2.sameElements(Array(3, 3, 5, 5)))
      assert(r3.sameElements(Array(3, 4, 3, 4, 6, 7, 6, 7)))
    }
    'inner {
      val r1: Array[Int] = BorderedRectangle.inner(m1)
      val r2: Array[Int] = BorderedRectangle.inner(m2)
      val r3: Array[Int] = BorderedRectangle.inner(m3)

      assert(r1.length == 0)
      assert(r2.sameElements(Array(4)))
      assert(r3.sameElements(Array(4, 5, 5, 6)))
    }
  }
}
