package igpolytech
import java.security.MessageDigest

object GeneralHelper {

  def generateHash(forString: String): String = {
    MessageDigest
      .getInstance("SHA-1")
      .digest(forString.getBytes("UTF-8"))
      .map("%02x".format(_))
      .mkString
  }

}
