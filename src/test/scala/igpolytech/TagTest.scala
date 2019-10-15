package igpolytech
import org.scalatest.FunSpec
import org.scalatest.Matchers
import java.io.File

class TagTest extends FunSpec with Matchers {
  override def withFixture(test: NoArgTest) = {
    try test()
    finally {
      if (new File(".sgit").exists()) FilesIO.delete(".sgit")
    }
  }

  describe("Tag creation") {
    it("should not create tag if already exists") {
      Repo.init(".")
      val repo = Repo(".sgit")
      FilesIO.write(s"${repo.tagsPath}superTag", "abc")

      val res = repo.createTag("superTag")

      res shouldBe "Sorry tag name is already used"
    }

    it("should create tag with head content inside") {
      Repo.init(".")
      val repo = Repo(".sgit")
      FilesIO.write(s".sgit${File.separator}HEAD", "headHash")

      val res = repo.createTag("superTag")

      res shouldBe "Tag superTag created"
      FilesIO.getContent(s"${repo.tagsPath}superTag") shouldBe "headHash"
    }

    it("should list the existing tags") {
      Repo.init(".")
      val repo = Repo(".sgit")
      FilesIO.write(s".sgit${File.separator}HEAD", "headHash")
      repo.createTag("superTag")

      val res = repo.listTags()

      assert(res.contains("superTag"))
    }

    it("should indicate no tags has been created") {
      Repo.init(".")
      val repo = Repo(".sgit")

      val res = repo.listTags()

      res shouldBe "No tags created. See sgit tag <name> to create one"
    }
  }
}
