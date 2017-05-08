package image_signature

import utest._
import com.letstalkdata.scalinear.Vector

object ImageSignatureTest extends TestSuite {
  def tests = TestSuite {
    'Something {
      true
      //new ImageSignature().something() == Vector(1, 2, 3)
    }


    //const s1 = [[2, 3], [1, 1], [1]] // ||s1|| = 4
    //const s2 = [[4, 2], [1, 2], [1]] // ||s2|| = sqrt(26)
    //const distance = imageSignature.distance(s1, s2)
    //const expected = Math.sqrt(4 + 1 + 0 + 1 + 0) / (4 + Math.sqrt(26))
    //assert.strictEqual(distance, expected)
    'distance {
      val s1 = new Signature(List(2, 3, 1, 1, 1)) // ||s1|| = 4
      val s2 = new Signature(List(4, 2, 1, 2, 1)) // ||s2|| = sqrt(26)
      val distance = s1.distance(s2)
      val expected = Math.sqrt(4 + 1 + 0 + 1 + 0) / (4 + Math.sqrt(26))

      assert(distance == expected)
    }
  }
}
