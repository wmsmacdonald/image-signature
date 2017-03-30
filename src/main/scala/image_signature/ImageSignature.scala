package image_signature

import java.nio.{ByteBuffer, IntBuffer}
import java.util

import scala.scalajs.js
import js.annotation.{JSExport, ScalaJSDefined}
import com.letstalkdata.scalinear.{Matrix, Vector}

@ScalaJSDefined
@JSExport("ImageSignature")
object ImageSignature extends js.Object {
  def generate(image: ImageData): Signature = {
    val data = image.data.toArray

    val rgbs = data.sliding(3, 4)

    // take average of RGB to get gray levels
    val grays: Iterator[Int] = rgbs.map(rgb => rgb.sum / rgb.length)

    val rowVectors = grays.sliding(image.width).map(row => Vector[Int](row:_*)).toSeq

    assert(rowVectors.length == image.height)

    val matrix = Matrix[Int](rowVectors:_*)

    val cropped: Matrix[Int] = Image.autoCrop(matrix, 10, 90)

    //val gridAverages = SignatureCalc.computeGridAverages(cropped, 10, 10)

    val numBlocksHigh = 10
    val numBlocksWide = 10

    val coords = SignatureCalc.getGridCoords(cropped.rows, cropped.cols, numBlocksHigh, numBlocksWide)

    val squares = SignatureCalc.getSquares(cropped, coords, numBlocksHigh, numBlocksWide)

    // take average of squares
    val squareAverages: List[Int] = squares.map(
      (a: Matrix[Int]) => MatrixCalc.sum(a).asArray.head / a.size
    )

    // turn square averages into a numBlocksHigh - 1 by numBlocksWide - 1 matrix
    val rows = squareAverages.sliding(squareAverages.length / squareAverages.length)
    Matrix(rows.map(xs => Vector(xs:_*)).toSeq:_*)

    new Signature(Seq(Seq(1)))
  }
  def distance(): Int = {
    5
  }
}


