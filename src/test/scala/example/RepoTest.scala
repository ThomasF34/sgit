package example

import java.io.File
import org.scalatest._

class RepoTest extends FlatSpec with Matchers {
  "Dirs and Files" should "be created after init" in {
    new Repo().init()
    assert(new File(".sgit/STAGE").exists())
    assert(new File(".sgit/HEAD").exists())
    assert(new File(".sgit/tags").exists())
    assert(new File(".sgit/commits").exists())
    assert(new File(".sgit/trees").exists())
    assert(new File(".sgit/blobs").exists())
    assert(new File(".sgit/branches").exists())
  }
}
