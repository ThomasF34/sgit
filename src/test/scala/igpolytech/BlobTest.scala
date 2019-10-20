package igpolytech
import java.security.MessageDigest
import org.scalatest._
import org.scalatest.Matchers
import scala.collection.mutable

class BlobTest extends FunSpec with Matchers {
  describe("Blob generating") {
    it("should generate an hash based on content") {
      def getFakeContent = () => "toInfinityAndBeyond"
      val blob1 = Blob("A113", getFakeContent)
      val blob2 = Blob("A114", getFakeContent)

      blob1.hash shouldBe blob2.hash
    }

    it("should use sha1 function to generate a hash") {
      def getFakeContent = () => "toInfinityAndBeyond"
      val blob = Blob("A113", getFakeContent)

      val expectedHash = MessageDigest
        .getInstance("SHA-1")
        .digest(blob.content.getBytes("UTF-8"))
        .map("%02x".format(_))
        .mkString

      blob.hash shouldBe expectedHash
    }

    it("should save hashed blob to repo") {
      val fakeRepoDir = mutable.Map[String, String]()
      val mockSave = fakeRepoDir(_) = _
      val blob = Blob("A113", () => "toInfinityAndBeyond")

      blob.save(mockSave)

      assert(fakeRepoDir.get(blob.hash).isDefined)
      fakeRepoDir.get(blob.hash).get shouldBe blob.content
    }

    it("should save blob to repo") {
      val fakeWorkingDir = mutable.Map[String, String]()
      val mockWrite = fakeWorkingDir(_) = _
      val blob = Blob("A113", () => "toInfinityAndBeyond")

      blob.writeBlob(mockWrite)

      assert(fakeWorkingDir.get(blob.name).isDefined)
      fakeWorkingDir.get(blob.name).get shouldBe blob.content
    }

    it("should make the diff between hashed blob and working dir file") {
      val blob = Blob("A113", () => "toInfinityAndBeyond")
      val fakeRepoDir = mutable.Map[String, String]((blob.hash, blob.content))
      val fakeWorkingDir = mutable.Map[String, String](
        (blob.name, "toInfinityAndBeyong\nJohn Lasseter")
      )
      val mockBlobContent = (hash: String) => fakeRepoDir.get(hash).get
      val mockFileContent = (name: String) => fakeWorkingDir.get(name).get
      val mockBlobExists = fakeWorkingDir.contains(_)

      val res = blob.getDiffWithNew(
        "dirTest",
        mockBlobExists,
        mockBlobContent,
        mockFileContent
      )

      res.contains(
        Diff(Array(Change(ChangeType.ADD, "John Lasseter", 2)), "A113")
      )
    }

    it("should indicate blob is removed from working dir") {
      val blob = Blob("A113", () => "toInfinityAndBeyond")
      val fakeRepoDir = mutable.Map[String, String]((blob.hash, blob.content))
      val fakeWorkingDir = mutable.Map[String, String]()
      val mockBlobContent = (hash: String) => fakeRepoDir.get(hash).get
      val mockFileContent = (name: String) => fakeWorkingDir.get(name).get
      val mockBlobExists = fakeWorkingDir.contains(_)

      val res = blob.getDiffWithNew(
        "dirTest",
        mockBlobExists,
        mockBlobContent,
        mockFileContent
      )

      assert(res.isDefined)
      assert(
        res.get.changes
          .contains(Change(ChangeType.SUB, "toInfinityAndBeyond", 1))
      )
    }
  }

  it("should generate blob from content") {
    val blob = Blob("A113", () => "toInfinityAndBeyond")
    val fakeRepoDir = mutable.Map[String, String]((blob.hash, blob.content))
    val mockBlobContent = (hash: String) => fakeRepoDir.get(hash).get

    val res = Blob.getBlob("A113", blob.hash, mockBlobContent)

    assert(res.name.equals(blob.name))
    assert(res.content.equals(blob.content))
  }
}
