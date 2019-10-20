package igpolytech
import org.scalatest.FunSpec
import org.scalatest.Matchers
import java.io.File
import scala.collection.mutable
import scala.xml.Node

class TagTest extends FunSpec with Matchers {
  describe("Tag saving") {
    it("should save to repo Dir") {
      val tag = Tag("A113", "hash")
      val fakeTagRepoDir =
        mutable.Map[String, String]((tag.name, tag.hash))
      val mockSaveTag =
        (name: String, hash: String) => fakeTagRepoDir(name) = hash

      tag.save(mockSaveTag)

      assert(fakeTagRepoDir.contains(tag.name))
      fakeTagRepoDir(tag.name) shouldBe tag.hash
    }

    it("should return true if tag exists") {
      val tag = Tag("A113", "hash")
      val fakeTagRepoDir =
        mutable.Map[String, String]((tag.name, tag.hash))
      val mockTagExists = fakeTagRepoDir.contains(_)

      Tag.exists(tag.name, mockTagExists) shouldBe true
    }

    it("should return false if tag not exists") {
      val fakeTagRepoDir =
        mutable.Map[String, String]()
      val mockTagExists = fakeTagRepoDir.contains(_)

      Tag.exists("test", mockTagExists) shouldBe false
    }

    it("should return Tag if exists") {
      val tag = Tag("A113", "hash")
      val fakeTagRepoDir =
        mutable.Map[String, String]((tag.name, tag.hash))
      val mockTagExists = fakeTagRepoDir.contains(_)
      val mockTagContent = fakeTagRepoDir(_)

      val res = Tag.fromName(tag.name, mockTagExists, mockTagContent)

      assert(res.isDefined)
      res.get.hash shouldBe tag.hash
      res.get.name shouldBe tag.name
    }

    it("should return None if tag does not exists") {
      val fakeTagRepoDir =
        mutable.Map[String, String]()
      val mockTagExists = fakeTagRepoDir.contains(_)
      val mockTagContent = fakeTagRepoDir(_)

      val res = Tag.fromName("test", mockTagExists, mockTagContent)

      res shouldBe None
    }
  }

  describe("Tag exploration") {
    it("should list all tags") {
      val tag = Tag("A113", "hash")
      val tag2 = Tag("A114", "hash2")
      val fakeTagRepoDir =
        mutable.Map[String, String](
          (tag.name, tag.hash),
          (tag2.name, tag2.hash)
        )
      val mockTagContent = fakeTagRepoDir(_)

      val res = Tag.allTags(Array("A113", "A114"), mockTagContent)

      assert(res.contains(tag))
      assert(res.contains(tag2))
    }

    it("Return associated commit if exists") {
      val commit = Commit("", "", "text", "author")
      val tag = Tag("A113", commit.hash)
      val fakeCommitRepoDir =
        mutable.Map[String, Node](
          (commit.hash, commit.toXml())
        )
      val mockCommitContent = fakeCommitRepoDir(_)
      val mockCommitExists = fakeCommitRepoDir.contains(_)

      val res = tag.getCommit(mockCommitContent, mockCommitExists)

      assert(res.isDefined)
      res.get.hash shouldBe commit.hash
    }

    it("Return None if associated commit does not exist") {
      val tag = Tag("A113", "test")
      val fakeCommitRepoDir =
        mutable.Map[String, Node]()
      val mockCommitContent = fakeCommitRepoDir(_)
      val mockCommitExists = fakeCommitRepoDir.contains(_)

      val res = tag.getCommit(mockCommitContent, mockCommitExists)

      res shouldBe None
    }
  }

  implicit val ioManager = IOManager()

  override def withFixture(test: NoArgTest) = {
    try test()
    finally {
      if (new File(".sgit").exists()) ioManager.delete(".sgit")
    }
  }

  describe("Tag Integration test") {
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

    it("should indicate no tags has been created") {
      Repo.init(".")
      val repo = Repo(".sgit")

      val res = repo.listTags()

      res shouldBe "No tags created. See sgit tag <name> to create one"
    }
  }
}
