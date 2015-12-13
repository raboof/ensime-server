package org.ensime.core

import java.io.File

import akka.event.slf4j.SLF4JLogging
import org.ensime.api._
import org.ensime.fixture._
import org.ensime.indexer.EnsimeVFS
import org.scalatest._
import org.ensime.util.file._

import scala.collection.immutable.Queue
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.reflect.internal.util.{ BatchSourceFile, OffsetPosition, RangePosition }
import scala.tools.nsc.Settings
import scala.tools.nsc.reporters.ConsoleReporter
import scala.util.Properties

class RichPresentationCompilerThatNeedsJavaLibsSpec extends WordSpec with Matchers
    with IsolatedRichPresentationCompilerFixture
    with RichPresentationCompilerTestUtils
    with ReallyRichPresentationCompilerFixture
    with SLF4JLogging {

  val original = EnsimeConfigFixture.SimpleTestProject

  "RichPresentationCompiler" should {
    "locate source position of Java classes in import statements" in withPresCompiler { (config, cc) =>
      import ReallyRichPresentationCompilerFixture._

      cc.search.refreshResolver()
      Await.result(cc.search.refresh(), 180.seconds)

      runForPositionInCompiledSource(config, cc,
        "package com.example",
        "import java.io.File@0@") { (p, label, cc) =>
          val sym = cc.askSymbolInfoAt(p).get
          assert(sym.declPos.isDefined)
          assert(sym.declPos.get match {
            case LineSourcePosition(f, i) =>
              f.parts.contains("File.java") && i > 0
            case _ => false
          })
        }
    }
  }
}

class RichPresentationCompilerSpec extends WordSpec with Matchers
    with IsolatedRichPresentationCompilerFixture
    with RichPresentationCompilerTestUtils
    with ReallyRichPresentationCompilerFixture
    with SLF4JLogging {

  val original = EnsimeConfigFixture.EmptyTestProject

  "RichPresentationCompiler" should {
    "round-trip between typeFullName and askTypeInfoByName" in withPresCompiler { (config, cc) =>
      val file = srcFile(config, "abc.scala", contents(
        "package com.example",
        "object /*1*/A { ",
        "   val /*1.1*/x: Int = 1",
        "   class  /*1.2*/X {} ",
        "   object /*1.3*/X {} ",
        "}",
        "class  /*2*/A { ",
        "   class  /*2.1*/X {} ",
        "   object /*2.2*/X {} ",
        "}"
      ))
      cc.askReloadFile(file)
      cc.askLoadedTyped(file)

      def roundtrip(label: String, expectedFullName: String) = {
        val comment = "/*" + label + "*/"
        val index = file.content.mkString.indexOf(comment)
        val tpe = cc.askTypeInfoAt(new OffsetPosition(file, index + comment.length)).get
        val fullName = tpe.fullName
        assert(fullName === expectedFullName)

        val tpe2 = cc.askTypeInfoByName(fullName).get
        assert(tpe2.fullName === expectedFullName)
      }

      roundtrip("1", "com.example.A$")
      roundtrip("1.1", "scala.Int")
      roundtrip("1.2", "com.example.A$$X")
      roundtrip("1.3", "com.example.A$$X$")
      roundtrip("2", "com.example.A")
      roundtrip("2.1", "com.example.A$X")
      roundtrip("2.2", "com.example.A$X$")
    }

    "get symbol info with cursor immediately after and before symbol" in withPresCompiler { (config, cc) =>
      import ReallyRichPresentationCompilerFixture._
      runForPositionInCompiledSource(config, cc,
        "package com.example",
        "object @0@Bla@1@ { def !(x:Int) = x }",
        "object Abc { def main { Bla @2@!@3@ 0 } }") { (p, label, cc) =>
          val sym = cc.askSymbolInfoAt(p).get
          assert(sym.declPos.isDefined)
          assert(sym.declPos.get match {
            case OffsetSourcePosition(f, i) => i > 0
            case _ => false
          })
        }
    }

    "find source declaration of standard operator" in withPresCompiler { (config, cc) =>
      import ReallyRichPresentationCompilerFixture._

      cc.search.refreshResolver()
      Await.result(cc.search.refresh(), 180.seconds)

      runForPositionInCompiledSource(config, cc,
        "package com.example",
        "object Bla { val x = 1 @0@+ 1 }") { (p, label, cc) =>
          val sym = cc.askSymbolInfoAt(p).get
          assert(sym.declPos.isDefined)
          assert(sym.declPos.get match {
            case OffsetSourcePosition(f, i) =>
              f.parts.contains("Int.scala") && i > 0
            case _ => false
          })
        }
    }

    "get symbol info by name" in withPresCompiler { (config, cc) =>
      def verify(
        fqn: String, member: Option[String], sig: Option[String], expectedLocalName: String,
        expectedName: String, expectedDeclAs: DeclaredAs
      ) = {
        val symOpt = cc.askSymbolByName(fqn, member, sig)
        assert(symOpt.isDefined)
        val sym = symOpt.get
        assert(sym.localName === expectedLocalName)
        assert(sym.name === expectedName)
        assert(sym.tpe.declaredAs === expectedDeclAs)
      }
      verify("java.lang.String", Some("startsWith"), None, "startsWith", "startsWith", DeclaredAs.Nil)
      verify("java.lang.String", None, None, "String", "java.lang.String", DeclaredAs.Class)
      // TODO: should not be getting 'nil for declaredAs.
      verify("scala.Option", Some("wait"), Some("(x$1: Long,x$2: Int): Unit"),
        "wait", "wait", DeclaredAs.Nil)
      verify("scala.Option", Some("wait"), Some("(x$1: Long): Unit"),
        "wait", "wait", DeclaredAs.Nil)
      verify("scala.Option", Some("wait"), Some("(): Unit"), "wait", "wait", DeclaredAs.Nil)
      verify("java$", Some("lang"), None, "lang", "java.lang$", DeclaredAs.Object)
      verify("scala$", Some("Option$"), None, "Option", "scala.Option$", DeclaredAs.Object)
      verify("scala$", Some("Option"), None, "Option", "scala.Option", DeclaredAs.Class)
      verify("scala.Option", Some("WithFilter"), None, "WithFilter", "scala.Option$WithFilter", DeclaredAs.Class)
      verify("scala.Option$WithFilter", Some("flatMap"), None, "flatMap", "flatMap", DeclaredAs.Nil)
      verify("scala.Boolean", None, None, "Boolean", "scala.Boolean", DeclaredAs.Class)
      verify("scala.Predef$", Some("DummyImplicit$"), None, "DummyImplicit", "scala.Predef$$DummyImplicit$", DeclaredAs.Object)
    }

    "handle askSymbolByName" in withPresCompiler { (config, cc) =>
      val file = srcFile(config, "abc.scala", contents(
        "package com.example",
        "object A { ",
        "   val x: Int = 1",
        "   class  X {}",
        "   object X {}",
        "}",
        "class A { ",
        "   class  X {}",
        "   object X {}",
        "}"
      ))
      cc.askReloadFile(file)
      cc.askLoadedTyped(file)

      def test(typeName: String, memberName: String, localName: String,
        symFullName: String, isType: Boolean, expectedTypeName: String, expectedDeclAs: DeclaredAs) = {
        val sym = cc.askSymbolByName(typeName, Some(memberName), None).get
        assert(sym.localName === localName)
        assert(sym.name === symFullName)
        assert(sym.tpe.fullName === expectedTypeName)
        assert(sym.tpe.declaredAs === expectedDeclAs)
        sym.tpe.declaredAs
      }

      test("com.example$", "A$", "A", "com.example.A$", isType = false, "com.example.A$", DeclaredAs.Object)
      test("com.example.A$", "x", "x", "x", isType = false, "scala.Int", DeclaredAs.Class)
      test("com.example.A$", "X$", "X", "com.example.A$$X$", isType = false, "com.example.A$$X$", DeclaredAs.Object)
      test("com.example.A$", "X", "X", "com.example.A$$X", isType = true, "com.example.A$$X", DeclaredAs.Class)

      test("com.example$", "A", "A", "com.example.A", isType = true, "com.example.A", DeclaredAs.Class)
      test("com.example.A", "X$", "X", "com.example.A$X$", isType = false, "com.example.A$X$", DeclaredAs.Object)
      test("com.example.A", "X", "X", "com.example.A$X", isType = true, "com.example.A$X", DeclaredAs.Class)
    }

    "get completions on member with no prefix" in withPosInCompiledSource(
      "package com.example",
      "object A { def aMethod(a: Int) = a }",
      "object B { val x = A.@@ "
    ) { (p, cc) =>
        val result = cc.completionsAt(p, 10, caseSens = false)
        result.completions should not be empty
        result.completions.head.name shouldBe "aMethod"
      }

    "won't try to complete the declaration containing point" in withPosInCompiledSource(
      "package com.example",
      "object Ab@@c {}"
    ) { (p, cc) =>
        val result = cc.completionsAt(p, 10, caseSens = false)
        assert(!result.completions.exists(_.name == "Abc"))
      }

    "get completions on a member with a prefix" in withPosInCompiledSource(
      "package com.example",
      "object A { def aMethod(a: Int) = a }",
      "object B { val x = A.aMeth@@ }"
    ) { (p, cc) =>
        val result = cc.completionsAt(p, 10, caseSens = false)
        assert(result.completions.length == 1)
        assert(result.completions.head.name == "aMethod")
      }

    "get completions on an object name" in withPosInCompiledSource(
      "package com.example",
      "object Abc { def aMethod(a: Int) = a }",
      "object B { val x = Ab@@ }"
    ) { (p, cc) =>
        val result = cc.completionsAt(p, 10, caseSens = false)
        assert(result.completions.length > 1)
        assert(result.completions.head.name == "Abc")
      }

    "get members for infix method call" in withPosInCompiledSource(
      "package com.example",
      "object Abc { def aMethod(a: Int) = a }",
      "object B { val x = Abc aM@@ }"
    ) { (p, cc) =>
        val result = cc.completionsAt(p, 10, caseSens = false)
        assert(result.completions.head.name == "aMethod")
      }

    "get members for infix method call without prefix" in withPosInCompiledSource(
      "package com.example",
      "object Abc { def aMethod(a: Int) = a }",
      "object B { val x = Abc @@ }"
    ) { (p, cc) =>
        val result = cc.completionsAt(p, 10, caseSens = false)
        assert(result.completions.head.name == "aMethod")
      }

    "complete multi-character infix operator" in withPosInCompiledSource(
      "package com.example",
      "object B { val l = Nil; val ll = l +@@ }"
    ) { (p, cc) =>
        val result = cc.completionsAt(p, 10, caseSens = false)
        assert(result.completions.exists(_.name == "++"))
      }

    "complete top level import" in withPosInCompiledSource(
      "package com.example",
      "import ja@@"
    ) { (p, cc) =>
        val result = cc.completionsAt(p, 10, caseSens = false)
        assert(result.completions.exists(_.name == "java"))
      }

    "complete sub-import" in withPosInCompiledSource(
      "package com.example",
      "import java.ut@@"
    ) { (p, cc) =>
        val result = cc.completionsAt(p, 10, caseSens = false)
        assert(result.completions.exists(_.name == "util"))
      }

    "complete multi-import" in withPosInCompiledSource(
      "package com.example",
      "import java.util.{ V@@ }"
    ) { (p, cc) =>
        val result = cc.completionsAt(p, 10, caseSens = false)
        assert(result.completions.exists(_.name == "Vector"))
      }

    "complete new construction" in withPosInCompiledSource(
      "package com.example",
      "import java.util.Vector",
      "object A { def main { new V@@ } }"
    ) { (p, cc) =>
        val result = cc.completionsAt(p, 10, caseSens = false)
        assert(result.completions.exists(m => m.name == "Vector" && m.isCallable))
      }

    "complete symbol in logical op" in withPosInCompiledSource(
      "package com.example",
      "object A { val apple = true; true || app@@ }"
    ) { (p, cc) =>
        val result = cc.completionsAt(p, 10, caseSens = false)
        assert(result.completions.exists(_.name == "apple"))
      }

    "complete infix method of Set." in withPosInCompiledSource(
      "package com.example",
      "object A { val t = Set[String](\"a\", \"b\"); t @@ }"
    ) { (p, cc) =>
        val result = cc.completionsAt(p, 10, caseSens = false)
        assert(result.completions.exists(_.name == "seq"))
        assert(result.completions.exists(_.name == "|"))
        assert(result.completions.exists(_.name == "&"))
      }

    "complete interpolated variables in strings" in withPosInCompiledSource(
      "package com.example",
      "object Abc { def aMethod(a: Int) = a }",
      s"""object B { val x = s"hello there, $${Abc.aMe@@}"}"""
    ) { (p, cc) =>
        val result = cc.completionsAt(p, 10, caseSens = false)
        assert(result.completions.head.name == "aMethod")
      }

    "not attempt to complete symbols in strings" in withPosInCompiledSource(
      "package com.example",
      "object Abc { def aMethod(a: Int) = a }",
      "object B { val x = \"hello there Ab@@\"}"
    ) { (p, cc) =>
        val result = cc.completionsAt(p, 10, caseSens = false)
        assert(result.completions.isEmpty)
      }

    "show all type arguments in the inspector." in withPosInCompiledSource(
      "package com.example",
      "class A { ",
      "def banana(p: List[String]): List[String] = p",
      "def pineapple: List[String] = List(\"spiky\")",
      "}",
      "object Main { def main { val my@@A = new A() }}"
    ) { (p, cc) =>
        val info = cc.askInspectTypeAt(p).get
        val sup = info.supers.find(sup => sup.tpe.name == "A").get;
        {
          val mem = sup.tpe.members.find(_.name == "banana").get.asInstanceOf[NamedTypeMemberInfo]
          val tpe = mem.tpe.asInstanceOf[ArrowTypeInfo]
          assert(tpe.resultType.name == "List")
          assert(tpe.resultType.args.head.name == "String")
          val (paramName, paramTpe) = tpe.paramSections.head.params.head
          assert(paramName == "p")
          assert(paramTpe.name == "List")
          assert(paramTpe.args.head.name == "String")
        }
        {
          val mem = sup.tpe.members.find(_.name == "pineapple").get.asInstanceOf[NamedTypeMemberInfo]
          val tpe = mem.tpe.asInstanceOf[BasicTypeInfo]
          assert(tpe.name == "List")
          assert(tpe.args.head.name == "String")
        }
      }

    "show classes without visible members in the inspector" in withPosInCompiledSource(
      "package com.example",
      "trait bidon { }",
      "case class pi@@po extends bidon { }"
    ) { (p, cc) =>
        val info = cc.askInspectTypeAt(p)
        val supers = info.map(_.supers).getOrElse(List())
        val supersNames = supers.map(_.tpe.name).toList
        assert(supersNames.toSet === Set("pipo", "bidon", "Object", "Product", "Serializable", "Any"))
      }

    "get type info for imports" in withPresCompiler { (config, cc) =>
      val expected = Map(
        "1.0" -> Some("java$"),
        "1.1" -> Some("java$"),
        "1.2" -> Some("java.io$"),
        "1.3" -> Some("java.io$"),
        "1.4" -> Some("java.io.File$"),
        "1.5" -> Some("java.io.File$"),
        "1.6" -> Some("java.lang.String"),
        "1.7" -> Some("java.lang.String"),
        "2.0" -> Some("java.io.Bits$"),
        "2.1" -> Some("java.io.Bits$"),
        "2.2" -> Some("java.io.File$"),
        "2.3" -> Some("java.io.File$"),
        "3.0" -> None
      )

      import ReallyRichPresentationCompilerFixture._
      runForPositionInCompiledSource(config, cc,
        "package com.example",
        "import @1.0@java@1.1@.@1.2@io@1.3@.@1.4@File@1.5@.@1.6@separator@1.7@",
        "import java.io.{ @2.0@Bits => one@2.1@, @2.2@File => two@2.3@ }",
        "import java.io._@3.0@") { (p, label, cc) =>
          val info = cc.askTypeInfoAt(p)
          assert(info.map { _.fullName } === expected(label))
        }
    }

    "get symbol positions for compiled files" in withPresCompiler { (config, cc) =>
      val defsFile = srcFile(config, "com/example/defs.scala", contents(
        "package com.example",
        "object /*1*/A { ",
        "   val /*1.1*/x: Int = 1",
        "   class /*1.2*/X {} ",
        "}",
        "class  /*2*/B { ",
        "   val /*2.1*/y: Int = 1",
        "   def /*2.2*/meth(a: String): Int = 1",
        "   def /*2.3*/meth(a: Int): Int = 1",
        "}",
        "trait  /*3*/C { ",
        "   val /*3.1*/z: Int = 1",
        "   class /*3.2*/Z {}",
        "}",
        "class /*4*/D extends C { }",
        "package object /*5.0*/pkg {",
        "   type /*5.1*/A  = java.lang.Error",
        "   def /*5.2*/B = 1",
        "   val /*5.3*/C = 2",
        "   class /*5.4*/D {}",
        "   object /*5.5*/E {}",
        "}"
      ), write = true)
      val usesFile = srcFile(config, "com/example/uses.scala", contents(
        "package com.example",
        "object Test { ",
        "   val x_1 = A/*1*/",
        "   val x_1_1 = A.x/*1.1*/",
        "   val x_1_2 = new A.X/*1.2*/",
        "   val x_2 = new B/*2*/",
        "   val x_2_1 = new B().y/*2.1*/",
        "   val x_2_2 = new B().meth/*2.2*/(\"x\")",
        "   val x_2_3 = new B().meth/*2.3*/(1)",
        "   val x_3: C/*3*/ = new D/*4*/",
        "   val x_3_1 = x_3.z/*3.1*/",
        "   val x_3_2 = new x_3.Z/*3.2*/",
        "   val x_5_0 = pkg.`package`/*5.0*/",
        "   var x_5_1: pkg.A/*5.1*/ = null",
        "   val x_5_2 = pkg.B/*5.2*/",
        "   val x_5_3 = pkg.C/*5.3*/",
        "   val x_5_4 = new pkg.D/*5.4*/",
        "   val x_5_5 = pkg.E/*5.5*/",
        "}"
      ))

      def test(label: String, cc: RichPresentationCompiler) = {
        val comment = "/*" + label + "*/"
        val defPos = defsFile.content.mkString.indexOf(comment) + comment.length
        val usePos = usesFile.content.mkString.indexOf(comment) - 1

        // Create a fresh pres. compiler unaffected by previous tests:
        // finding a symbol's position loads the source into the compiler
        // which changes its state.

        implicit val ensimeVFS = EnsimeVFS()
        val cc1 = new RichPresentationCompiler(cc.config, cc.settings, cc.reporter, cc.parent, cc.indexer, cc.search)

        try {
          cc1.askReloadFile(usesFile)
          cc1.askLoadedTyped(usesFile)

          val info = cc1.askSymbolInfoAt(new OffsetPosition(usesFile, usePos)) match {
            case Some(x) => x
            case None => fail(s"For $comment, askSymbolInfoAt returned None")
          }
          val declPos = info.declPos
          declPos match {
            case Some(op: OffsetSourcePosition) => assert(op.offset === defPos)
            case _ => fail(s"For $comment, unexpected declPos value: $declPos")
          }
        } finally {
          cc1.askShutdown()
          ensimeVFS.close()
        }
      }

      compileScala(
        List(defsFile.path),
        config.subprojects.head.targetDirs.head,
        cc.settings.classpath.value
      )

      cc.search.refreshResolver()
      Await.result(cc.search.refresh(), 180.seconds)

      val scalaVersion = scala.util.Properties.versionNumberString
      val parts = scalaVersion.split("\\.").map { _.toInt }
      if (parts(0) > 2 || (parts(0) == 2 && parts(1) > 10)) {
        /* in Scala 2.10, the declaration position of "pacakge object" is
           different so we just skip this test */
        test("5.0", cc)
      }

      List(
        "1", "1.1", "1.2", "2", "2.1", "2.2", "2.3", "3", "3.1", "3.2", "4",
        "5.1", "5.2", "5.3", "5.4", "5.5"
      ).
        foreach(test(_, cc))
    }

    "get symbol info after getting implicit information" in withPresCompiler { (config, cc) =>
      val source = contents(
        "package foo",
        "import scala.concurrent.duration._",
        "import scala.language.postfixOps",
        "object Example {",
        "  2 seconds",
        "}\n"
      )

      val fileOnDisk = srcFile(config, "foo/Example.scala", source)
      cc.askReloadFile(fileOnDisk)
      cc.askLoadedTyped(fileOnDisk)

      // Initialize the search service
      cc.search.refreshResolver()
      Await.result(cc.search.refresh(), 180.seconds)

      // 'hover over seconds'
      val secondsSymbolPosition = new OffsetPosition(fileOnDisk, source.indexOf("onds"))
      val symbolInfo1 = cc.askSymbolInfoAt(secondsSymbolPosition).get
      assert(symbolInfo1.name == "seconds")

      // 'mark implicts'
      val filename = config.subprojects.head.sourceRoots.head / "foo" / "Example.scala"
      val sourceFile = cc.createSourceFile(
        SourceFileInfo(filename, Some(source))
      )
      cc.askReloadFile(sourceFile)
      cc.askNotifyWhenReady()
      cc.askLoadedTyped(sourceFile)

      val range = new RangePosition(fileOnDisk, 0, 0, source.length - 1)
      println(s"Asking implicit info ${range}")
      val info = cc.askImplicitInfoInRegion(range)

      // 'hover over seconds'
      val sym2 = cc.askSymbolInfoAt(secondsSymbolPosition)
      assert(sym2.get.name == "seconds")
    }
  }
}

trait RichPresentationCompilerTestUtils {
  val scala210 = Properties.versionNumberString.startsWith("2.10")

  def compileScala(paths: List[String], target: File, classPath: String): Unit = {
    val settings = new Settings
    settings.outputDirs.setSingleOutput(target.getAbsolutePath)
    val reporter = new ConsoleReporter(settings)
    settings.classpath.value = classPath
    val g = new scala.tools.nsc.Global(settings, reporter)
    val run = new g.Run
    run.compile(paths)
  }

  def srcFile(proj: EnsimeConfig, name: String, content: String, write: Boolean = false, encoding: String = "UTF-8"): BatchSourceFile = {
    val src = proj.subprojects.head.sourceRoots.head / name
    if (write) {
      src.createWithParents()
      scala.tools.nsc.io.File(src)(encoding).writeAll(content)
    }
    new BatchSourceFile(src.getPath, content)
  }

  def readSrcFile(src: BatchSourceFile, encoding: String = "UTF-8"): String =
    scala.tools.nsc.io.File(src.path)(encoding).slurp()

  def contents(lines: String*) = lines.mkString("\n")
}

trait ReallyRichPresentationCompilerFixture {
  this: RichPresentationCompilerFixture with RichPresentationCompilerTestUtils =>

  // conveniences for accessing the fixtures
  final def withPresCompiler(
    testCode: (EnsimeConfig, RichPresentationCompiler) => Any
  ): Any =
    withRichPresentationCompiler { (_, c, cc) => testCode(c, cc) }

  // final def withPosInCompiledSource(lines: String*)(testCode: (OffsetPosition, RichPresentationCompiler) => Any) =
  //   withPosInCompiledSource{ (p, _, pc) => testCode(p, pc) }

  final def withPosInCompiledSource(lines: String*)(testCode: (OffsetPosition, RichPresentationCompiler) => Any): Any =
    withPresCompiler { (config, cc) =>
      import ReallyRichPresentationCompilerFixture._
      runForPositionInCompiledSource(config, cc, lines: _*) {
        (p, _, cc) => testCode(p, cc)
      }
    }

}

object ReallyRichPresentationCompilerFixture
    extends RichPresentationCompilerTestUtils {
  def runForPositionInCompiledSource(config: EnsimeConfig, cc: RichPresentationCompiler, lines: String*)(testCode: (OffsetPosition, String, RichPresentationCompiler) => Any): Any = {
    val contents = lines.mkString("\n")
    var offset = 0
    var points = Queue.empty[(Int, String)]
    val re = """@([a-z0-9\.]*)@"""
    re.r.findAllMatchIn(contents).foreach { m =>
      points :+= ((m.start - offset, m.group(1)))
      offset += (m.end - m.start)
    }
    val file = srcFile(config, "def.scala", contents.replaceAll(re, ""))
    cc.askReloadFile(file)
    cc.askLoadedTyped(file)
    assert(points.nonEmpty)
    for (pt <- points) {
      testCode(new OffsetPosition(file, pt._1), pt._2, cc)
    }
  }
}
