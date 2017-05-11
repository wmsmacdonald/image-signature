package image_signature

import utest._
import scala.scalajs.js

object ImageSignatureTest extends TestSuite {
  def tests = TestSuite {

    //const s1 = [[2, 3], [1, 1], [1]] // ||s1|| = 4
    //const s2 = [[4, 2], [1, 2], [1]] // ||s2|| = sqrt(26)
    //const distance = imageSignature.distance(s1, s2)
    //const expected = Math.sqrt(4 + 1 + 0 + 1 + 0) / (4 + Math.sqrt(26))
    //assert.strictEqual(distance, expected)
    'distance {
      val s1 = js.Array(2, 3, 1, 1, 1) // ||s1|| = 4
      val s2 = js.Array(4, 2, 1, 2, 1) // ||s2|| = sqrt(26)
      val distance = ImageSignature.distance(s1, s2)
      val expected = Math.sqrt(4 + 1 + 0 + 1 + 0) / (4 + Math.sqrt(26))

      assert(distance == expected)
    }
  }
}
