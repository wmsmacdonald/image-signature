package image_signature

import com.letstalkdata.scalinear.{ Matrix, Vector }
import utest._

object ImageTest extends TestSuite {
  def tests = TestSuite {
    'autoCrop {
      // [ 0 1 2 3 ... 10 ]
      // [ 1 2 3 4 ... 11 ]
      // [ 2 3 4 5 ... 12 ]
      // ...
      // [ 10 11 12... 20 ]
      val rows: IndexedSeq[Vector[Int]] = (0 to 10).map(n => Vector(n to n + 10:_*))
      val m: Matrix[Int] = Matrix[Int](rows:_*)
      val cropped: Matrix[Int] = Image.autoCrop(m, 10, 90)

      val rows2: IndexedSeq[Vector[Int]] = (0 to 1000).map(n => Vector(n to n + 1000:_*))
      val m2: Matrix[Int] = Matrix[Int](rows2:_*)
      val cropped2: Matrix[Int] = Image.autoCrop(m2, 10, 90)

      assert(cropped.rows == 9)
      assert(cropped.cols == 9)
      // top left corner
      assert(cropped(0, 0) == 2)
      // top right corner
      assert(cropped(0, 8) == 10)
      // bottom left corner
      assert(cropped(8, 0) == 10)
      // bottom right corner
      assert(cropped(8, 8) == 18)
    }

    'getSoftenedSquare {
      // [ 0 1 2 3 ... 10 ]
      // [ 1 2 3 4 ... 11 ]
      // [ 2 3 4 5 ... 12 ]
      // ...
      // [ 10 11 12... 20 ]
      val rows: IndexedSeq[Vector[Int]] = (0 to 10).map(n => Vector(n to n + 10:_*))
      val m: Matrix[Int] = Matrix[Int](rows:_*)

      // TODO
    }
  }
}
