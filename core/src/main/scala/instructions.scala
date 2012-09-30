package com.github.philcali.puppet
import actions._

import dispatch._

import lmxml.{ LmxmlNode, TextNode, ParsedNode, SinglePass }

import java.io.{ File, FileOutputStream }

object Instructions {
  lazy val defaultSet: Map[String, Action] = Map(
    "go" -> GoAction,
    "post" -> PostAction,
    "submit" -> SubmitAction,
    "source" -> SourceAction,
    "find" -> FindAction,
    "set" -> SetAction,
    "each" -> EachAction,
    "[each-item]" -> EachItemAction,
    "download" -> DownloadAction,
    "println" -> PrintAction
  )

  lazy val Default = Instructions(defaultSet)

  def apply(actions: (String, Action)*) = new Instructions(Map(actions:_*))
}

case class Instructions(actions: Map[String, Action])
  extends SinglePass[Promise[List[String]]] {

  type Nodes = Seq[ParsedNode]

  def single(node: ParsedNode) = node match {
    case LmxmlNode(name, attrs, children) if name == "browser" =>
      val config = PuppetConfig.fromLmxml(children,
        attrs.get("file").map(PuppetConfig.fromFile(_))
          .getOrElse(PuppetConfig.default)
      )

      val browser = PuppetClient(config.build)
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
      val ((nNode, nCtx), nLog) = actions.get(node.name) match {
        case Some(fun) => fun(node, cl, ctx)
        case _ => node -> ctx -> None
      }

      for {
        first <- instructions(cl, nNode.children, nCtx)
        second <- instructions(cl, ns, ctx)
      } yield {
        nLog.map(List(_)).getOrElse(Nil) ::: first ::: second
      }
    case TextNode(data, _, child) :: ns if !data.isEmpty =>
      println(data)
      instructions(cl, ns, ctx)
    case lmxml.CommentNode(_) :: ns => instructions(cl, ns, ctx)
    case n :: ns =>
      for {
        first <- instructions(cl, n.children, ctx)
        second <- instructions(cl, ns, ctx)
      } yield (first ::: second)
    case Nil => Promise(Nil)
  }
}
