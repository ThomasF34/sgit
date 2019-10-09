package igpolytech

import java.io.File
import org.scalatest._

class RepoTest extends FunSpec {
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
  }

  describe("Commit handling") {
    it("shouldn't get commit from empty HEAD") {
      Repo.init(".")
      val repo = Repo(".sgit")
      FilesIO.write(s".sgit${File.separator}HEAD", "")
      assert(repo.getLastCommit().isEmpty)
    }

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
  describe("Stage handling") {
    pending
  }
}
