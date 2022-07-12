package e01_01

import cats.effect.unsafe.implicits.global
import sttp.client3._

object Runner {

  def main(args: Array[String]): Unit = {
    {
      val uri     = uri"http://www.baidu.com"
      val result1 = Base.requestAction(uri)
      val result2 = Compare.request(uri)
      val compareAction = for {
        r1 <- result1
        r2 <- result2
      } yield (r1.contains("百度"), r2.contains("百度"))

      val compareResult = compareAction.unsafeRunSync()
      assert(compareResult._1 == true)
      assert(compareResult._2 == true)
    }
  }

}
