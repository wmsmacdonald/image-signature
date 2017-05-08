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

    val rowVectors = grays.sliding(image.width, image.width).map(row => Vector[Int](row:_*)).toSeq

    assert(rowVectors.length == image.height, "image dimension not correct")

    val matrix = Matrix[Int](rowVectors:_*)

    val cropped: Matrix[Int] = Image.autoCrop(matrix, 10, 90)

    val numBlocksHigh = 10
    val numBlocksWide = 10

    val coords = SignatureCalc.getGridCoords(cropped.rows, cropped.cols, numBlocksHigh, numBlocksWide)

    // from Goldberg paper
    // first we get a P x P square centered at grid point
    val P: Int = math.max(2, math.floor(0.5 + math.min(cropped.rows, cropped.cols) / 20).toInt)

    // number of pixes bottom/left of grid point
    val lowerOffset: Int = math.floor((P - 1.0) / 2).toInt
    // number of pixels right/top of grid point
    val upperOffset: Int = math.ceil((P - 1.0) / 2).toInt

    val squareAverages: List[Int] = coords.map { case (r, c) =>
      Image.softenedSquareAvg(cropped, r, c, lowerOffset, upperOffset)
    }.map(Math.round).map(_.toInt)

    // turn square averages into a numBlocksHigh - 1 by numBlocksWide - 1 matrix
    val avgMatrix: Matrix[Int] = MatrixCalc.shape(squareAverages, numBlocksHigh - 1, numBlocksWide - 1)

    // neighbors of grid points
    val neighborGroups = MatrixCalc.indexes(avgMatrix).map { case (i, j) =>
      MatrixCalc.getNeighbors(avgMatrix, i, j, 1, 1)
    }

    // differences between averages and neighbor averages
    val differenceGroups: List[Array[Int]] = squareAverages.zip(neighborGroups).map {
      case (avg, neighbors) => neighbors.map(_ - avg)
    }

    val normalizer = SignatureCalc.computeNormalizer(differenceGroups)

    // normalize all differences
    val normalized = differenceGroups.map(diffs => diffs.map(normalizer))
    new Signature(normalized.flatten)
  }
}


