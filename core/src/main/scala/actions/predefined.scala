package com.github.philcali.puppet
package actions

import utils.Params
import lmxml.{ LmxmlNode, TextNode, ParsedNode, SinglePass }

import dispatch._
import css.query.default._

import java.io.{ File, PrintStream, FileOutputStream, BufferedInputStream }

import com.ning.http.multipart.FilePart

case class ActionContext(node: LmxmlNode, cl: Http, ctx: Context) {
  def basicReturn(newContext: Context, log: Option[String] = None) = {
    ActionReturn(node, newContext, log)
  }
}

case class ActionReturn(node: LmxmlNode, ctx: Context, log: Option[String])

object Action {
  type Handler = PartialFunction[ActionContext, ActionReturn]

  def apply(handler: Handler) = new Action {
    def perform = handler
  }
}

trait Action {
  def perform: Action.Handler

  def stripAttrs(attrs: Map[String, String], default: String) = {
    val key = attrs.get("name").getOrElse(default)
    val attr = attrs.get("value").filter(_ startsWith "@")

    (key -> attr)
  }
}

/**
 * Caches response into context
 *
 * source: access NodeSeq with @source
 */
object SourceAction extends Action {
  def perform = {
    case action @ ActionContext(_, _, ctx) =>
      val source = ctx.response.map(as.TagSoup).getOrElse(xml.NodeSeq.Empty)
      action basicReturn (ctx + ("source" -> source))
  }
}

/**
 * Submits a form
 *
 * submit @to="form.action" @method="get"
 * submit @form="form#gbqf" @q="lmxml"
 * submit @form="form" @file-params="data.json"
 */
object SubmitAction extends Action {
  def perform = {
    case action @ ActionContext(node, _, ctx) =>
      val form = node.attrs.get("form").getOrElse("form")

      val nForm = ctx.get[xml.NodeSeq]("source")
        .orElse(ctx.response.map(as.TagSoup))
        .map(_ ? form getOrElse xml.NodeSeq.Empty)

      val to = nForm.map(_ \ "@action" text)
        .getOrElse(node.attrs.getOrElse("to", ""))

      val method = nForm.map(_ ? "[method=post]" isDefined) match {
        case Some(true) => "POST"
        case _ => "GET"
      }

      // replace node with flattened to / action
      val copied = node.copy(
        attrs = node.attrs + ("to" -> to) + ("type" -> method) - "form"
      )

      GoAction perform (action.copy(node = copied))
  }
}

/**
 * Makes a simple Request
 *
 * go @to="example.com": makes a GET request
 * go @to="example.com" @base: makes a GET request, and stores this url as the
 * base request for future requests
 * go @to="example.com" @base @secure: makes a secured GET request
 * go @to="example.com" @type="PUT" @value="file|context"
 * go @to="example.com" @req-headers="data.json"
 */
object GoAction extends Action {
  val logResponse = "%s [%d] - %s"

  def perform = {
    case action @ ActionContext(node, cl, ctx) =>
      val (built, params) =
        ((SafeUrl(node, ctx), Map[String,String]()) /: NodeParams(node, ctx)) {
          case ((req, oldParams), (k, v)) if k == "req-headers" =>
            (req <:< Params.convert(v)) -> oldParams
          case ((req, oldParams), (k, v)) =>
            val file = new File(v)
            if (k.startsWith("file-upload-") && file.exists) {
              val filePart = new FilePart(
                k.stripPrefix("file-upload-"), file.getName, file
              )
              req.addBodyPart(filePart) -> oldParams
            } else {
              req -> (oldParams + (k -> v))
            }
        }

      val temp = node.attrs.get("base")
        .map(_ => ctx + ("base" -> built.url))
        .getOrElse(ctx)

      val verb = node.attrs.get("type").getOrElse("GET")

      (verb match {
        case "GET" =>
          cl(built <<? params > temp)
        case "POST" =>
          cl(built << params > temp)
        case "PUT" =>
          params.get("value").map(new File(_)).filter(_.exists).map { file =>
            cl(built <<< file > temp)
          } getOrElse {
            cl(built << params > temp)
          }
        case _ =>
          cl(built.subject.setMethod(verb) > temp)
      }) map { r =>
        val res = r.response.get
        action.basicReturn(r - "source", Some(
          logResponse.format (verb, res.getStatusCode, res.getUri.toURL)
        ))
      } apply()
  }
}

/**
 * Finds html node in source to be processed later
 *
 * find @by-css="#id .class > elem": attempts to use CSS selectors on source
 */
object FindAction extends Action {
  def perform = {
    case action @ ActionContext(node, _, ctx) =>
      val params = NodeParams(node, ctx)
      val byCss = params.get("by-css").getOrElse("*")
      val con = params.get("contains").map(_.r).getOrElse(""".*""".r)

      val results = ctx.get[xml.NodeSeq]("source")
        .orElse(ctx.response.map(as.TagSoup))
        .map(_ ? byCss getOrElse (xml.NodeSeq.Empty))
        .map(_ filter (n => con.findFirstMatchIn(n.text).isDefined))

      action.basicReturn(ctx unwrap ("find-results" -> results))
  }
}

/**
 * Sets a found result, or anything as a context value
 *
 * set @link="@href": Sets "link" -> "@href" in context
 * set @with-results @link="@href": Sets node matching "@href" in context
 */
object SetAction extends Action {
  def perform = {
    case action @ ActionContext(node, _, ctx) =>
      val params = NodeParams(node, ctx) - "with-results"
      val useNodes = (
        node.attrs.contains("with-results") &&
        ctx.get("find-results").isDefined
      )

      val nCtx = (ctx /: params)({
        case (c, (k, v)) =>
          val value = if (useNodes) {
            ctx.get[xml.NodeSeq]("find-results").get \ v text
          } else {
            v
          }
          c + (k -> value)
      })

      action basicReturn nCtx
  }
}

/**
 * Iterates over searched results
 *
 * each @value="@href": begins iteration on href attr of node
 * each @name="something": tags something to the text of item
 * each @name="something" @value="@href": tags something to item, and iterates
 * each @name="something" @value="@href" @take="3": grabs first three
 */
object EachAction extends Action {
  def perform = {
    case action @ ActionContext(node, _, ctx) =>
      val key = node.attrs.get("name").getOrElse(node.name)
      ActionReturn(node.copy(children = ctx.get[xml.NodeSeq]("find-results").map(ns =>
        node.attrs.get("take").map(_.toInt) match {
          case Some(index) if index > 0 => ns.take(index)
          case None => ns
        }).map(_.zipWithIndex.map {
          case (n, i) =>
            LmxmlNode(
              "[each-item]",
              Map("index" -> i.toString, "each-key" -> key) ++ node.attrs,
              node.children
            )
        }
      ).getOrElse(Nil)), ctx, None)
  }
}

/**
 * Helper action for EachAction
 * it loads the index of the iteration in the Context
 */
object EachItemAction extends Action {
  def perform = {
    case action @ ActionContext(node, _, ctx) =>
      action.basicReturn(
        ctx.get[xml.NodeSeq]("find-results").map { ns =>
          val index = node.attrs.get("index").map(_.toInt).getOrElse(0)
          val key = node.attrs.get("each-key").getOrElse("each")
          val (k, attr) = stripAttrs(node.attrs, key)
          val v = attr.map(ns(index) \ _).getOrElse(ns(index)).text
          ctx + (k -> v) + ("index" -> index)
        } getOrElse (ctx)
      )
  }
}

/**
 * Prints out data to stdout
 *
 * println: prints out source
 * println @context: prints out current context
 * println @context @value="@find-results": prints specific context value
 */
object PrintAction extends Action {
  def perform = {
    case action @ ActionContext(node, _, ctx) =>
      node.attrs.get("context") match {
        case Some(_) => node.attrs.get("value").map(_.stripPrefix("@")) match {
          case Some(key) => ctx.get(key).foreach(println)
          case None => println(ctx)
        }
        case None =>
          ctx.response.map(as.String).foreach(println)
      }
      action basicReturn ctx
  }
}

/**
 * Downloads response to file; uses file name from request
 *
 * download @to="location"
 * download @to="location" @name="file[@index].mp3"
 */
object DownloadAction extends Action {
  val types = Map("text" -> "UTF-8")

  def pump(in: java.io.InputStream, out: java.io.OutputStream): Unit = {
    val buffer = new Array[Byte](2048)
    in.read(buffer) match {
      case n if n >= 0 => out.write(buffer, 0, n); pump(in, out)
      case _ => out.close()
    }
  }

  def perform = {
    case action @ ActionContext(node, _, ctx) =>
      val params = NodeParamsEmbeded(node, ctx)
      val dest = params.get("to").map(new File(_)).getOrElse(new File("."))
      if (!dest.exists) dest.mkdirs

      ctx.response.foreach { res =>
        val str = res.getUri.toURL.getFile
        // TODO: fix this please
        val fromReq = try {
          str.split("/").takeRight(1).head
        } catch {
          case _ => "index.html"
        }
        val fileName = params.get("name")
        val file = new File(dest, fileName getOrElse fromReq)
        val fileType = types.get(params.get("type").getOrElse("binary"))

        val fs = new FileOutputStream(file)

        pump(
          new BufferedInputStream(res.getResponseBodyAsStream),
          fileType.map(cs => new PrintStream(fs)).getOrElse(fs)
        )
      }

      action basicReturn ctx
  }
}

/**
 * Runs a commandline option
 *
 * run @cmd="mkdir -p [read@ Create a directory >]"
 * run @cmd="spdf < source > page.pdf"
 * run @cmd="ls *.png" @name="local-pngs": To be used later
 */
object ConsoleAction extends Action {
  import java.io.{ ByteArrayInputStream => BAIS }
  import sys.process._

  case class CommanLine(ctx: Context) extends ConsoleParsers {
    override def redirectedIn: Parser[Builder] = "<" ~> command ^^ { cmd =>
      pro => ctx.data
                .get(cmd.trim).map(_.toString)
                .map(s => new BAIS(s.getBytes))
                .map(s => pro #< s)
                .getOrElse(pro #< Process(cmd))
    }
  }

  def perform = {
    case action @ ActionContext(node, _, ctx) =>
      val log = "[Console: %d] %s"
      val params = NodeParamsEmbeded(node, ctx)
      val parser = CommanLine(ctx)

      params.get("cmd").map(parser.execute).map {
        case (code, console) =>
          console.errorLines.foreach(System.err.println)
          val key = params.get("name").getOrElse("console")
          val note = Some(log format (code, params("cmd")))
          action.basicReturn(ctx + (key -> console.output), note)
      } getOrElse (action.basicReturn(ctx))
  }
}
