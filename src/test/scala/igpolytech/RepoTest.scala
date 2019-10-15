package igpolytech

import java.io.File
import org.scalatest._

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
      tree.save(
        repo.treesPath,
        repo.blobsPath
      )
      FilesIO.write(s".sgit${File.separator}STAGE", tree.hash)

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

      repo.setStage(tree)

      val hash = FilesIO.getHash(s".sgit${File.separator}STAGE")

      assert(hash.isDefined)
      assert(hash.get.equals(tree.hash))
      assert(
        Tree.getTree(repo.treesPath, repo.blobsPath, hash.get).equals(tree)
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
      Repo.init(".")
      val repo = Repo(".sgit")
      def getFakeContent = () => "toInfinityAndBeyond"
      val blob = new Blob("testFile", getFakeContent)
      val tree = new Tree("", Array(), Array(blob))
      repo.setStage(tree)
      FilesIO.write(".sgit/CommitMessage", "John Lasseter is great")
      repo.commit()
      val lastCommitHash = repo.getLastCommit().get.hash
      def newFakeContent = () => "shouldNotRemainAlive"
      val newTree = new Tree(
        "",
        Array(),
        Array(blob, new Blob("willBeDestroyed", newFakeContent))
      )
      repo.setStage(newTree)
      FilesIO.write(".sgit/CommitMessage", "This will be deleted")
      repo.commit()

      repo.checkout(lastCommitHash)

      FilesIO.fileExists("willBeDestroyed") shouldBe false
      FilesIO.fileExists("testFile") shouldBe true
    }
  }
}
