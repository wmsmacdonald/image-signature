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
      assert(s1.equals(Vector(0, 6)))
      assert(s2.equals(Vector(1, 5)))
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
      val m: Matrix[Int] = Matrix(Vector(1, 2, 3), Vector(4, 5, 6))
      val s1 = MatrixCalc.slice(m, 0, 1, 1, 3)
      val s2 = MatrixCalc.slice(m, 1, 2, 0, 1)
      assert(s1.equals(Matrix(Vector(2, 3))))
      assert(s2.equals(Matrix(Vector(4))))
    }
  }
}


