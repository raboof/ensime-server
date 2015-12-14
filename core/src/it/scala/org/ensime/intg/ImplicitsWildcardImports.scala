package org.ensime.intg

import akka.event.slf4j.SLF4JLogging
import org.ensime.api._
import org.ensime.fixture._
import org.scalatest.{ Matchers, WordSpec }
import org.ensime.util.file._

/**
 * This is a reproduction of https://github.com/ensime/ensime-server/issues/1176
 * which might be caused by https://github.com/scala/scala/pull/4777
 */
class ImplicitsWildcardImports extends WordSpec with Matchers
    with IsolatedEnsimeConfigFixture
    with IsolatedTestKitFixture
    with IsolatedProjectFixture
    with SLF4JLogging {

  val original = EnsimeConfigFixture.ImplicitsTestProject

  "ensime-server" should {
    "allow getting symbols also after marking implicits imported with a wildcard import" in {
      withEnsimeConfig { implicit config =>
        withTestKit { implicit testkit =>
          withProject { (project, asyncHelper) =>
            import testkit._

            val sourceRoot = scalaMain(config)
            val exampleFile = sourceRoot / "org/example/Example.scala"

            log.info("Getting type the first time")
            project ! SymbolAtPointReq(Left(exampleFile), 116)
            expectMsgType[Option[SymbolInfo]].get.name should be("seconds")

            log.info("Getting implicit info")
            project ! ImplicitInfoReq(Left(exampleFile), OffsetRange(0, 121))
            expectMsgType[ImplicitInfos]

            log.info("Getting type the second time")
            project ! SymbolAtPointReq(Left(exampleFile), 116)
            expectMsgType[Option[SymbolInfo]].get.name should be("seconds")
          }
        }
      }
    }
  }
}
