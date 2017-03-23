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

    val cropped = Image.autoCrop(matrix, 10, 90)

    new Signature(Seq(Seq(1)))
  }
  def distance(): Int = {
    5
  }
}
