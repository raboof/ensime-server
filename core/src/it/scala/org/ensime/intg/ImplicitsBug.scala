package org.ensime.intg

import akka.event.slf4j.SLF4JLogging
import org.ensime.api._
import org.ensime.fixture._
import org.scalatest.{ Matchers, WordSpec }
import org.ensime.util.file._

class ImplicitsBug extends WordSpec with Matchers
    with IsolatedEnsimeConfigFixture
    with IsolatedTestKitFixture
    with IsolatedProjectFixture
    with SLF4JLogging {

  val original = EnsimeConfigFixture.ImplicitsTestProject

  "ensime-server" should {
    "reproduce the side-effect occuring when marking implicits" in {
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
