package igpolytech
import java.security.MessageDigest
import org.scalatest._
import org.scalatest.Matchers

class BlobTest extends FunSpec with Matchers {
  describe("Blob generating") {
    it("should generate an hash based on content") {
      def getFakeContent = () => "toInfinityAndBeyond"
      val blob1 = new Blob("A113", getFakeContent)
      val blob2 = new Blob("A114", getFakeContent)

      blob1.hash shouldBe blob2.hash
    }

    it("should use sha1 function to generate a hash") {
      def getFakeContent = () => "toInfinityAndBeyond"
      val blob = new Blob("A113", getFakeContent)

      val expectedHash = MessageDigest
        .getInstance("SHA-1")
        .digest(blob.content.getBytes("UTF-8"))
        .map("%02x".format(_))
        .mkString

      blob.hash shouldBe expectedHash
    }
  }
}
