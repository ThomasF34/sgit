package igpolytech

import java.io.File
import org.scalatest._
import scala.xml.Node

class RepoTest extends FunSpec with Matchers {
  override def withFixture(test: NoArgTest) = {
    try test()
    finally {
      if (new File("../.sgit").exists()) FilesIO.delete("../.sgit")
      if (new File("test").exists()) FilesIO.delete("test")
      if (new File(".sgit").exists()) FilesIO.delete(".sgit")
    }
  }

  describe("Init") {
    it("should create dirs and files after init") {
      val res = Repo.init(".")
      assert(res == "Repo initialized")
      assert(new File(".sgit/STAGE").exists())
      assert(new File(".sgit/HEAD").exists())
      assert(new File(".sgit/tags").exists())
      assert(new File(".sgit/commits").exists())
      assert(new File(".sgit/trees").exists())
      assert(new File(".sgit/blobs").exists())
      assert(new File(".sgit/branches").exists())
    }

    it("should be initialized in a given path") {
      new File("./test").mkdir()
      val res = Repo.init("./test")
      assert(res == "Repo initialized")
      assert(new File("test/.sgit/STAGE").exists())
      assert(new File("test/.sgit/HEAD").exists())
      assert(new File("test/.sgit/tags").exists())
      assert(new File("test/.sgit/commits").exists())
      assert(new File("test/.sgit/trees").exists())
      assert(new File("test/.sgit/blobs").exists())
      assert(new File("test/.sgit/branches").exists())
    }

    it("shouldn't be initialized if already exists in current directory") {
      Repo.init(".")
      val res = Repo.init(".")
      assert(res == "Sorry, a sgit repository is already initialized")
    }

    it("should get empty head when initialized") {
      Repo.init(".")
      val repo: Repo = new Repo(".sgit")

      repo.head.mode shouldBe "branch"
      repo.head.content shouldBe "master"
    }
  }

  describe("Stage handling") {
    it("should get Tree from STAGE") {
      Repo.init(".")
      val repo = Repo(".sgit")
      val tree = Tree("A113")
      val writeBlobToRepo = (content: String, hash: String) =>
        FilesIO.write(repo.blobsPath)(hash, content)
      val saveTreeAsXml =
        (xml: Node, hash: String) => FilesIO.saveXml(repo.treesPath)(xml, hash)
      tree.save(
        saveTreeAsXml,
        writeBlobToRepo
      )
      FilesIO.write(repo.stageDir)(repo.stageFile, tree.hash)

      val stageTree = repo.getStage

      assert(stageTree.isDefined)
      assert(stageTree.get.equals(tree))
    }

    it("shouldn't get tree from empty STAGE") {
      Repo.init(".")
      val repo = Repo(".sgit")

      val stageTree = repo.getStage

      assert(stageTree.isEmpty)
    }

    it("should set STAGE from a tree") {
      Repo.init(".")
      val repo = Repo(".sgit")
      val tree = Tree("A113")
      //TODO DELETE ME
      val blobContent =
        (blobHash: String) => FilesIO.getContent(s"${repo.blobsPath}$blobHash")
      val treeContent =
        (treeHash: String) => FilesIO.loadXml(s"${repo.treesPath}$treeHash")

      repo.setStage(tree)

      val hash = FilesIO.getHash(s".sgit${File.separator}STAGE")

      assert(hash.isDefined)
      assert(hash.get.equals(tree.hash))
      assert(
        Tree
          .getTree(
            hash.get,
            blobContent,
            treeContent
          )
          .equals(tree)
      )
    }
  }

  describe("Checkout handling") {
    it("should not checkout if modified files") {
      pending
    }

    it("should not checkout if untracked files") {
      pending
    }

    it("shoud remove old and create new files") {
      pending
    }
  }
}
