package igpolytech
import java.security.MessageDigest
import org.scalatest._
import org.scalatest.Matchers
import scala.xml.Node

class TreeTest extends FunSpec with Matchers {
  describe("Tree generating") {
    // it(
    //   "should generate a XML element based on containing trees, blobs and own name"
    // ) {
    //   val expectedXml: Node =
    //     <Tree>
    //       <name>A113</name>
    //       <trees>

    //       </trees>
    //       <blobs>

    //       </blobs>
    //      </Tree>
    //   assert(new Tree("A113").toXml().equals(expectedXml))
    // }

    it("should generate hash base on concatenation of name trees and blobs") {
      val emptyTree = new Tree("A113")
      val emptyTree2 = new Tree("A114")
      val treeWithTree = new Tree("A114", Array(emptyTree2))
      val treeWithBlob = new Tree(
        "A114",
        Array(emptyTree2),
        Array(new Blob("unoBlobo", () => "Italian"))
      )
      emptyTree.hash shouldNot be(emptyTree2.hash)
      emptyTree2.hash shouldNot be(treeWithTree.hash)
      treeWithTree.hash shouldNot be(treeWithBlob.hash)
    }
  }

  describe("Tree merging") {
    it("should merge two trees") {
      val tree = new Tree("A113")
      val blob = new Blob("unoBlobo", () => "Italian")
      val blob2 = new Blob("duoBlobi", () => "Italian again")
      val treeWithBlob = new Tree("A114", Array(tree), Array(blob))
      val treeWithBlob2 = new Tree(
        "A114",
        Array(tree),
        Array(blob2)
      )

      val mergedTree = treeWithBlob.mergeTree(treeWithBlob2)

      val expectedTree = new Tree("A114", Array(tree), Array(blob, blob2))
      assert(mergedTree.equals(expectedTree))
    }
  }

  describe("Tree exploration") {
    it("should return all file from a tree") {
      pending
    }
  }
}
