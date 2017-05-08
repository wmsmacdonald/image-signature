package image_signature

import scala.scalajs.js
import scala.scalajs.js.annotation.ScalaJSDefined


@ScalaJSDefined
class Signature(val comparisons: List[Int]) extends js.Object {
  //TODO
  def mask(rows: Range, columns: Range): Signature = {
    this
  }

  def length: Int = comparisons.length

  // from Goldberg paper
  def distance(b: Signature): Double = {
    def l2Norm(a: List[Int]): Double = Math.sqrt(a.map(Math.pow(_, 2)).sum)

    l2Norm(
      // elementwise subtraction
      (comparisons, b.comparisons).zipped.map(_ - _)
    ) / (
      l2Norm(comparisons) + l2Norm(b.comparisons)
    )
  }
}


