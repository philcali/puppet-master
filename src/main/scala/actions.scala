package com.github.philcali.puppet
package actions

import utils.Params

import lmxml.{ LmxmlNode, TextNode, ParsedNode, SinglePass }

import dispatch._
import css.query.default._

import java.io.{ File, FileOutputStream }
import java.net.URLEncoder
import java.net.URI

import util.parsing.combinator.RegexParsers

trait Action extends Function3[LmxmlNode, PuppetClient, Context, (Instructions => Promise[List[String]])] {
  def stripAttrs(attrs: Map[String, String], default: String) = {
    val key = attrs.get("name").getOrElse(default)
    val attr = attrs.get("value").filter(_ startsWith "@")

    (key -> attr)
  }
}

trait UrlParser { self: Action =>
  private val http = """[a-zA-Z]{2,5}://""".r

  private val unsafe = Map(
    ' ' -> "%20",
    '"' -> "%22",
    '<' -> "%3C", '>' -> "%3E",
    '#' -> "%23",
    '%' -> "%25",
    '{' -> "%7B", '}' -> "%7D",
    '|' -> "%7C",
    '\\' -> "%5C",
    '~' -> "%7E",
    '^' -> "%5E",
    '[' -> "%5B", "]" -> "%5D",
    '`' -> "%60"
  )

  private val reserved = Map(
    '$' -> "%24",
    '&' -> "%26",
    '+' -> "%2B",
    ',' -> "%2C",
    '/' -> "%2F",
    ':' -> "%3A",
    ';' -> "%3B",
    '=' -> "%3D",
    '?' -> "%3F",
    '@' -> "%40"
  )

  def encodeAll(s: String) = {
    s.map(c =>
      unsafe.get(c).getOrElse(reserved.get(c).getOrElse(c.toString))).mkString
  }

  def encode(s: String) =
    s.map(c => unsafe.get(c).getOrElse(c.toString)).mkString

  def makeUrl(node: LmxmlNode, ctx: Context) = {
    val evaled = url(node.attrs.get("to")
      .map(to => ctx.get(to).getOrElse(to))
      .map(to => ctx.response.map(_.getUri).map { uri =>
        if (http.findFirstMatchIn(to).filter(_.start == 0).isEmpty) {
          // No way to know if URL is valid unless parsed
          try {
            ctx.get("base").map(new URI(_)).getOrElse(uri).resolve(to).toString
          } catch {
            case _ =>
              // Attempt to properly encode url's (form vs url)
              val str = ctx.get("base").getOrElse(uri.toURL.toString)
              str.stripSuffix("/") + "/" +
              (to.stripPrefix("/").split("/").toList.reverse match {
                case head :: tail => encode(head) :: tail.map(encodeAll)
                case Nil => Nil
              }).reverse.mkString("/")
          }
        } else to
      }.getOrElse(to)).get)
    val cookied = (evaled /: ctx.cookies.values)(_ addCookie _)
    if (ctx.data.contains("secure")) cookied.secure else cookied
  }
}

trait ParamParser extends RegexParsers { self: Action =>
  protected val reservedKeys = List("to", "base", "secure")

  type Yank = (String, Context) => String

  val reader = new tools.jline.console.ConsoleReader()

  val ident = """[a-zA-Z0-9_\-]+""".r

  val everything = """[^@]+""".r

  def fromCss: Parser[Yank] = "css@" ~> everything ~ opt("@" ~> ident) ^^ {
    case selector ~ filter => (value, ctx) => {
      val rtn = ctx.get[xml.NodeSeq]("source").map(
        _ ? selector getOrElse (xml.NodeSeq.Empty))
      filter.map (attr =>
        rtn map (_ \ ("@" + attr) text)
      ).getOrElse(
        rtn map (_ text)
      ).getOrElse(
        value
      )
    }
  }

  def fromPrompt: Parser[Yank] = "read@" ~> everything ~ opt("@" ~> ident) ^^ {
    case prompt ~ ctxKey => (value, ctx) => {
      ctxKey.map(ctx.get[String](_)).filter(_.isDefined).map(_.get).getOrElse(
        reader.readLine(prompt)
      )
    }
  }

  def fromPass: Parser[Yank] = "pass@" ~> everything ^^ (
    prompt => (value, ctx) => reader.readLine(prompt, '*')
  )

  def options = (fromCss | fromPrompt | fromPass)

  def yank(input: String): Yank = parseAll(options, input) match {
    case Success(fun, _) => fun
    case _ => (value, ctx) =>
      if (value.startsWith("@")) {
        ctx.get(value.stripPrefix("@")).map(_.toString).getOrElse(value)
      } else value
  }

  def parseParams(node: LmxmlNode, ctx: Context) = {
    val submitted = (node.attrs /: reservedKeys)(_ - _)

    (submitted.get("file") match {
      case Some(file) if Params.validate(file) =>
        Params.convert(file) ++ (submitted - "file")
      case _ => submitted
    }).map {
      case (k, v) => k -> yank(v).apply(v, ctx)
    }
  }
}

/**
 * Caches response into context
 *
 * source: access NodeSeq with @source
 */
object SourceAction extends Action {
  def apply(node: LmxmlNode, cl: PuppetClient, ctx: Context) = parent => {
    val source = ctx.response.map(as.TagSoup).getOrElse(xml.NodeSeq.Empty)

    parent.instructions(cl, node.children, ctx + ("source" -> source))
  }
}

/**
 * Submits a form
 *
 * submit @to="form.action" @method="get"
 * submit @form="form#gbqf" @q="lmxml"
 * submit @form="form" @file="data.json"
 */
object SubmitAction extends Action with UrlParser with ParamParser {
  def apply(node: LmxmlNode, cl: PuppetClient, ctx: Context) = {
    val form = node.attrs.get("form").getOrElse("form")

    val nForm = ctx.get[xml.NodeSeq]("source")
      .map(_ ? form getOrElse xml.NodeSeq.Empty)

    val to = nForm.map(_ \ "@action" text)
      .getOrElse(node.attrs.getOrElse("to", ""))

    // replace node with flattened to / action
    val copied = node.copy(attrs = node.attrs + ("to" -> to) - "form")

    nForm.map(_ ? "[method=post]" isDefined) match {
      case Some(true) => PostAction(copied, cl, ctx)
      case _ => GoAction(copied, cl, ctx)
    }
  }
}

/**
 * Makes a POST to a url
 *
 * post @to="form.action" {
 *   username: "pcali1",
 *   password: "*******"
 * }
 * post @to="form.action" @username="@username"
 * post @to="form.action" @file="submission.properties"
 * post @to="form.action" @file="data.json"
 */
object PostAction extends Action with UrlParser with ParamParser {
  val logResponse = "POST [%d] - %s"

  def apply(node: LmxmlNode, cl: PuppetClient, ctx: Context) = parent => {
    for {
      r <- cl(makeUrl(node, ctx) << parseParams(node, ctx) > ctx)
      lines <- parent.instructions(cl, node.children, r - "source")
    } yield {
      val res = r.response.get
      logResponse.format (res.getStatusCode, res.getUri.toURL) :: lines
    }
  }
}

/**
 * Makes a simple GET
 *
 * go @to="example.com": makes a GET request
 * go @to="example.com" @base: makes a GET request, and stores this url as the
 * base request for future requests
 * go @to="example.com" @base @secure: makes a secured GET request
 */
object GoAction extends Action with UrlParser with ParamParser {
  val logResponse = "GET [%d] - %s"

  def apply(node: LmxmlNode, cl: PuppetClient, ctx: Context) = parent => {
    val theUrl = makeUrl(node, ctx)

    val temp = node.attrs.get("base")
      .map(_ => ctx + ("base" -> theUrl.url))
      .getOrElse(ctx)

    for {
      r <- cl(theUrl <<? parseParams(node, temp) > temp)
      lines <- parent.instructions(cl, node.children, r - "source")
    } yield {
      val res = r.response.get
      logResponse.format (res.getStatusCode, res.getUri.toURL) :: lines
    }
  }
}

/**
 * Finds html node in source to be processed later
 *
 * find @by-css="#id .class > elem": attempts to use CSS selectors on source
 */
object FindAction extends Action {
  def apply(node: LmxmlNode, cl: PuppetClient, ctx: Context) = parent => {
    val byCss = node.attrs.get("by-css").getOrElse("*")
    val con = node.attrs.get("contains").map(_.r).getOrElse(""".*""".r)

    val results = ctx.get[xml.NodeSeq]("source")
      .orElse(ctx.response.map(as.TagSoup))
      .map(_ ? byCss getOrElse (xml.NodeSeq.Empty))
      .map(_ filter (n => con.findFirstMatchIn(n.text).isDefined))

    parent.instructions(cl, node.children, ctx unwrap ("find-results" -> results))
  }
}

/**
 * Sets a found result as a context value
 *
 * set @value="@href": sets found result attribute as context value
 * set @name="something" @value="@href": tags value with a specific name
 */
object SetAction extends Action {
  def apply(node: LmxmlNode, cl: PuppetClient, ctx: Context) = parent => {
    val (key, attr) = stripAttrs(node.attrs, node.name)
    val value = ctx.get[xml.NodeSeq]("find-results").map(
      n => attr.map(n \ _).getOrElse(n).text)

    parent.instructions(cl, node.children, ctx + (key -> value.getOrElse("")))
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
  def apply(node: LmxmlNode, cl: PuppetClient, ctx: Context) = parent => {
    ctx.get[xml.NodeSeq]("find-results").map(ns =>
      node.attrs.get("take").map(_.toInt) match {
        case Some(index) if index > 0 => ns.take(index)
        case None => ns
      }).map({
        case xs if xs.headOption.isDefined =>
          val (k, attr) = stripAttrs(node.attrs, node.name)
          val v = attr.map(xs.head \ _).getOrElse(xs.head).text
          for {
            s1 <- parent.instructions(cl, node.children, ctx + (k -> v))
            s2 <- parent.instructions(cl, Seq(node), ctx + ("find-results" -> xs.tail))
          } yield (s1 ::: s2)
        case _ => Promise(Nil)
      }
    ).getOrElse(Promise(Nil))
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
  def apply(node: LmxmlNode, cl: PuppetClient, ctx: Context) = parent => {
    node.attrs.get("context") match {
      case Some(_) => node.attrs.get("value").map(_.stripPrefix("@")) match {
        case Some(key) => ctx.get(key).foreach(println)
        case None => println(ctx)
      }
      case None => ctx.response.map(as.String).foreach(println)
    }
    parent.instructions(cl, node.children, ctx)
  }
}

/**
 * Downloads response to file; uses file name from request
 *
 * download @to="location"
 */
object DownloadAction extends Action {
  def pump(in: java.io.InputStream, out: java.io.OutputStream): Unit = {
    val buffer = new Array[Byte](1024)
    in.read(buffer) match {
      case n if n >= 0 => out.write(buffer, 0, n); pump(in, out)
      case _ => out.close()
    }
  }

  def apply(node: LmxmlNode, cl: PuppetClient, ctx: Context) = parent => {
    val dest = node.attrs.get("to").map(new File(_)).getOrElse(new File("."))
    if (!dest.exists) dest.mkdirs

    ctx.response.foreach { res =>
      val str = res.getUri.toURL.getFile
      val file = new File(dest, str.split("/").takeRight(1).head)
      pump(res.getResponseBodyAsStream, new FileOutputStream(file))
    }

    parent.instructions(cl, node.children, ctx)
  }
}
