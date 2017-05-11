package image_signature

import com.letstalkdata.scalinear.{ Matrix, Vector }
import utest._

object ImageTest extends TestSuite {
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

    'softenedSquareAvg {
      Image.softenedSquareAvg(m1, centerRow = 1, centerCol = 1,
        lowerOffset = 0, upperOffset = 1) ==> 3.0

      Image.softenedSquareAvg(m2, centerRow = 2, centerCol = 2,
        lowerOffset = 1, upperOffset = 1) ==> 4.0

      Image.softenedSquareAvg(m3, centerRow = 2, centerCol = 2,
        lowerOffset = 1, upperOffset = 2) ==> 5.0
    }

    'weightedAvg {
      Image.weightedAvg(m1) ==> 3.0
      Image.weightedAvg(m2) ==> 4.0
      Image.weightedAvg(m3) ==> 5.0
    }
  }
}
