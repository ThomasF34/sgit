package igpolytech
import org.scalatest.FunSpec
import org.scalatest.Matchers
import java.io.File

class TagTest extends FunSpec with Matchers {
  implicit val ioManager = IOManager()

  override def withFixture(test: NoArgTest) = {
    try test()
    finally {
      if (new File(".sgit").exists()) ioManager.delete(".sgit")
    }
  }

  describe("Tag creation") {
    it("should not create tag if already exists") {
      Repo.init(".")
      val repo = Repo(".sgit")
      ioManager.write(repo.tagsPath)("superTag", "abc")

      val res = repo.createTag("superTag")

      res shouldBe "Sorry tag name is already used"
    }

    it("should not create tag if the head does not point to a specific commit") {
      Repo.init(".")
      val repo = Repo(".sgit")

      val res = repo.createTag("superTag")

      res shouldBe "The head your repository does not point towards a commit. Please commit before creating a tag"
    }

    it("should create tag with head content inside") {
      pending
      Repo.init(".")
      val repo = Repo(".sgit")
      repo.commit("toInfinityAndBeyond")
      repo.setHead("detached", "abc")

      val res = repo.createTag("superTag")

      res shouldBe "Tag superTag created"
      ioManager.getContent(repo.tagsPath)("superTag") shouldBe "test"
    }

    it("should list the existing tags") {
      Repo.init(".")
      val repo = Repo(".sgit")

      ioManager.write(repo.tagsPath)("superTag", "A113")

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
