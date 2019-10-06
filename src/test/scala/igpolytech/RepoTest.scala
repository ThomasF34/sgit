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

  describe("Repo") {
    describe("Init") {
      it("should create dirs and files after init") {
        val res = new Repo().init(".")
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
        val res = new Repo().init("./test")
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
        new Repo().init(".")
        val res = new Repo().init(".")
        assert(res == "Sorry, a sgit repository is already initialized")
      }
    }
  }
}
