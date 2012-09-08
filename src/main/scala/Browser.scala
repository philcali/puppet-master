package com.github.philcali.puppet
import actions._

import dispatch._

import lmxml.{ LmxmlNode, TextNode, ParsedNode, SinglePass }

import java.io.{ File, FileOutputStream }

object TestMan extends App {
  object Lmxml extends lmxml.PlainLmxmlFactory with lmxml.FileLoading

  val promises = Lmxml.fromFile("instructions.lmxml")(Instructions.Default)

  for {
    promise <- promises
    lines <- promise
  } {
    lines foreach println
  }
}

object Instructions {
  lazy val defaultSet: Map[String, Action] = Map(
    "go" -> GoAction,
    "find" -> FindAction,
    "set" -> SetAction,
    "each" -> EachAction,
    "download" -> DownloadAction,
    "println" -> PrintAction
  )

  lazy val Default = Instructions(defaultSet)

  def apply(actions: (String, Action)*) = new Instructions(Map(actions:_*))
}

case class Instructions(actions: Map[String, Action]) extends SinglePass[Promise[List[String]]] {

  type Nodes = Seq[ParsedNode]

  def single(node: ParsedNode) = node match {
    case LmxmlNode(name, attrs, children) if name == "browser" =>
      val config = attrs.get("file")
                        .map(PuppetConfig.fromFile(_))
                        .getOrElse(PuppetConfig.default)

      val browser = PuppetClient(PuppetConfig.fromLmxml(children, config).build)
      instructions(browser, children.tail, Context(Map.empty)).onComplete({
        case _ => browser.shutdown()
      })
  }

  def instructions(cl: PuppetClient, nodes: Nodes, ctx: Context): Promise[List[String]] = nodes match {
    case node :: ns if node.name == "instructions" =>
      for {
        first <- instructions(cl, node.children, ctx)
        second <- instructions(cl, ns, ctx)
      } yield (first ::: second)
    case (node: LmxmlNode) :: ns =>
      val head = actions.get(node.name) match {
        case Some(fun) => fun(node, cl, ctx).apply(this)
        case _ => Promise(Nil)
      }
      for {
        first <- head
        second <- instructions(cl, ns, ctx)
      } yield (first ::: second)
    case lmxml.CommentNode(_) :: ns => instructions(cl, ns, ctx)
    case node :: ns =>
      for {
        first <- instructions(cl, node.children, ctx)
        second <- instructions(cl, ns, ctx)
      } yield (first ::: second)
    case Nil => Promise(Nil)
  }
}
