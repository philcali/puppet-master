package com.github.philcali.puppet
import actions._

import dispatch._

import lmxml.{ LmxmlNode, TextNode, ParsedNode, CommentNode, SinglePass }

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
  extends SinglePass[List[String]] {

  type Nodes = Seq[ParsedNode]

  def single(node: ParsedNode) = node match {
    case LmxmlNode(name, attrs, children) if name == "browser" =>
      val browser = Http.configure { config =>
        PuppetConfig.fromLmxml(children,
          attrs.get("file").map(PuppetConfig.fromFile(_)).getOrElse(config)
        )
      }

      val logs = instructions(browser, children.tail, Context(Map.empty))
      browser.shutdown()
      logs
    case _ => throw new PuppetException("Must start with 'browser'")
  }

  def instructions(cl: Http, nodes: Nodes, ctx: Context): List[String] = nodes match {
    case node :: ns if node.name == "instructions" =>
      node.children.toList.flatMap(n => _instruction(cl, n, ctx)) :::
      instructions(cl, ns, ctx)
    case n :: ns =>
      _instruction(cl, n, ctx) ::: instructions(cl, ns, ctx)
    case Nil => Nil
  }

  private def _instruction(cl: Http, node: ParsedNode, ctx: Context): List[String] = node match {
    case lNode @ LmxmlNode(name, attrs, children) =>
      val ActionReturn(nNode, nCtx, nLog) = actions.get(name) match {
        case Some(act) => act perform (ActionContext(lNode, cl, ctx))
        case _ => ActionReturn(lNode, ctx, None)
      }
      nLog.map(List(_)).getOrElse(Nil) :::
      nNode.children.toList.flatMap(n => _instruction(cl, n, nCtx))
    case TextNode(data, _, child) => println(data); Nil
    case CommentNode(_) => Nil
    case _ => node.children.toList.flatMap(n => _instruction(cl, n, ctx))
  }

}
