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

    // from Goldberg paper
    // first we get a P x P square centered at grid point
    val P: Int = math.max(2, math.floor(0.5 + math.min(cropped.rows, cropped.cols) / 20).toInt)

    // if P = 4, upperOffset = 2 and lowerOffset = 1
    val upperOffset: Int = math.ceil((P - 1.0) / 2).toInt
    val lowerOffset: Int = math.floor((P - 1.0) / 2).toInt

    val squares = coords.map { case (r, c) =>
      MatrixCalc.getNeighbors(cropped, r, c, lowerOffset, upperOffset)
    }

    // take average of squares
    val squareAverages: List[Int] = squares.map(
      (a: Matrix[Int]) => MatrixCalc.sum(a).asArray.head / a.size
    )

    // turn square averages into a numBlocksHigh - 1 by numBlocksWide - 1 matrix
    val rows = squareAverages.sliding(squareAverages.length / squareAverages.length)
    val avgMatrix = Matrix(rows.map(xs => Vector(xs:_*)).toSeq:_*)

    val neighborGroups = for (i <- 0 until avgMatrix.rows; j <- 0 until avgMatrix.cols)
      yield MatrixCalc.getNeighbors(cropped, i, j, lowerOffset, upperOffset, includeSelf = true)
    val differentialGroups: List[Array[Int]] = squareAverages.zip(neighborGroups).map {
      case (avg, neighbors) => neighbors.map(n => n - avg)
    }

    val lighter = differentialGroups.flatten.filter(d => d > 2)
    val darker = differentialGroups.flatten.filter(d => d < -2)

    // get median as cutoff between lighter and much lighter
    val lighterCutoff: Int = lighter.sorted.apply(lighter.length / 2)
    // get median as cutoff between darker and much darker
    val darkerCutoff: Int = darker.sorted.apply(darker.length / 2)

    val normalizedWithCutoffs = SignatureCalc.normalizeValue(2, lighterCutoff, darkerCutoff)(_)
    normalizedWithCutoffs(2)

    new Signature(differentialGroups.map(diffs => diffs.map(normalizedWithCutoffs)))
  }
  def distance(): Int = {
    5
  }
}


