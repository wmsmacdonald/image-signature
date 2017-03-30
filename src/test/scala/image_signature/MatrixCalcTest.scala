package image_signature

import com.letstalkdata.scalinear.{ Matrix, Vector }
import utest._

object MatrixCalcTest extends TestSuite {
  def tests = TestSuite {
    'diff {
      val m: Matrix[Int] = Matrix(Vector(1, 3, 6, 10), Vector(0, 5, 6, 8))
      val d0: Matrix[Int] = MatrixCalc.diff(m, 0)
      val d1: Matrix[Int] = MatrixCalc.diff(m, 1)
      assert(d0.equals(Matrix(Vector(-1, 2, 0, -2))))
      assert(d1.equals(Matrix(Vector(2, 3, 4), Vector(5, 1, 2))))
    }

    'sum {
      val m: Matrix[Int] = Matrix(Vector(0, 1), Vector(0, 5))
      val s1: Vector[Int] = MatrixCalc.sum(m, 0)
      val s2: Vector[Int] = MatrixCalc.sum(m, 1)
      val s3: Vector[Int] = MatrixCalc.sum(m)
      assert(s1.equals(Vector(0, 6)))
      assert(s2.equals(Vector(1, 5)))
      assert(s3.equals(Vector(6)))
    }

    'cumsum_matrix {
      val m: Matrix[Int] = Matrix(Vector(1, 2, 3), Vector(4, 5, 6))
      val cs1: Matrix[Int] = MatrixCalc.cumsum(m)
      val cs2: Matrix[Int] = MatrixCalc.cumsum(m, 0)
      val cs3: Matrix[Int] = MatrixCalc.cumsum(m, 1)
      assert(cs1.equals(Matrix(Vector(1, 3, 6, 10, 15, 21))))
      assert(cs2.equals(Matrix(Vector(1, 2, 3), Vector(5, 7, 9))))
      assert(cs3.equals(Matrix(Vector(1, 3, 6), Vector(4, 9, 15))))
    }
    'cumsum_vector {
      val v: Vector[Int] = Vector(1, 2, 3, 4, 5, 6)
      val cs: Vector[Int] = MatrixCalc.cumsum(v)
      assert(cs.equals(Vector(1, 3, 6, 10, 15, 21)))
    }

    'searchsorted {
      val v: Vector[Int] = Vector(2, 3, 5, 10, 15)
      assert(MatrixCalc.searchsorted(v, 1) == 0)
      assert(MatrixCalc.searchsorted(v, 4) == 2)
      assert(MatrixCalc.searchsorted(v, 20) == 5)
    }

    'slice {
      // [1 2 3]
      // [4 5 6]
      val m: Matrix[Int] = Matrix(Vector(1, 2, 3), Vector(4, 5, 6))
      val s1 = MatrixCalc.slice(m, 0, 1, 1, 3)
      val s2 = MatrixCalc.slice(m, 1, 2, 0, 1)
      val s3 = MatrixCalc.slice(m, -1, 1, 0, 5)
      assert(s1.equals(Matrix(Vector(2, 3))))
      assert(s2.equals(Matrix(Vector(4))))
      assert(s3.equals(Matrix(Vector(1, 2, 3))))


      // [ 0 1 2 3 ... 10 ]
      // [ 1 2 3 4 ... 11 ]
      // [ 2 3 4 5 ... 12 ]
      // ...
      // [ 10 11 12... 20 ]
      val rows: IndexedSeq[Vector[Int]] = (0 to 10).map(n => Vector(n to n + 10:_*))
      val m2: Matrix[Int] = Matrix[Int](rows:_*)
      val sliced = MatrixCalc.slice(m2, 1, 10, 1, 10)
      assert(sliced.rows == 9)
      assert(sliced.cols == 9)
      // top left corner
      assert(sliced(0, 0) == 2)
      // top right corner
      assert(sliced(0, 8) == 10)
      // bottom left corner
      assert(sliced(8, 0) == 10)
      // bottom right corner
      assert(sliced(8, 8) == 18)
    }
  }
}


