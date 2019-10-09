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
      pending
    }
  }

  describe("Tree exploration") {
    it("should return all file from a tree") {
      pending
    }
  }
}
