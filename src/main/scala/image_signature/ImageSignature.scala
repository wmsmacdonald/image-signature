package image_signature

import java.nio.{ByteBuffer, IntBuffer}
import java.util

import scala.scalajs.js
import js.annotation.{JSExport, ScalaJSDefined}
import com.letstalkdata.scalinear.{Matrix, Vector}

import scala.collection.JavaConverters._
import js.JSConverters._
import js.typedarray.TA2AB
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.collection.JavaConversions

@ScalaJSDefined
@JSExport("ImageSignature")
object ImageSignature extends js.Object {
  def generate(image: ImageData): Signature = {
    val data = image.data.toArray

    val rgbs = data.sliding(3, 4)

    // take average of RGB to get gray levels
    val grays = rgbs.map(rgb => rgb.sum.toDouble / rgb.length)

    val rowVectors = grays.sliding(image.width).map(new Vector[Int](_))

    assert(rowVectors.length == image.height)

    val matrix = new Matrix[Int](rowVectors)

    new Signature(Seq(Seq(1)))
  }
  def distance(): Int = {
    5
  }
}
