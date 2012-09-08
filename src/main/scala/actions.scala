package com.github.philcali.puppet
package actions

import lmxml.{ LmxmlNode, TextNode, ParsedNode, SinglePass }

import dispatch._

import java.io.{ File, FileOutputStream }
import java.net.URLEncoder

trait Action extends Function3[LmxmlNode, PuppetClient, Context, (Instructions => Promise[List[String]])] {
  def stripAttrs(attrs: Map[String, String], default: String) = {
    val key = attrs.get("name").getOrElse(default)
    val attr = attrs.get("value").filter(_ startsWith "@")

    (key -> attr.getOrElse("@value"))
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
object GoAction extends Action {
  val logResponse = "[%d] - %s"

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

  def formEncode(s: String) = URLEncoder.encode(s, "utf-8")

  def encodeAll(s: String) = {
    s.map(c =>
      unsafe.get(c).getOrElse(reserved.get(c).getOrElse(c.toString))).mkString
  }

  def encode(s: String) =
    s.map(c => unsafe.get(c).getOrElse(c.toString)).mkString

  def apply(node: LmxmlNode, cl: PuppetClient, ctx: Context) = parent => {
    val evaled = node.attrs.get("to")
      .map(to => ctx.get(to).getOrElse(to))
      .map(to => ctx.response.map(_.getUri).map { uri =>
        if (!to.contains("://")) {
          val str = ctx.get("base").getOrElse(uri.toURL.toString)
          val tURL = str.stripSuffix("/") + "/" + to.stripPrefix("/")
          // No way to know if URL is valid unless parsed
          try {
            new java.net.URI(tURL).toString
          } catch {
            case _ =>
              // Attempt to properly encode url's (form vs url)
              str.stripSuffix("/") + "/" +
              (to.stripPrefix("/").split("/").toList.reverse match {
                case head :: tail => encode(head) :: tail.map(encodeAll)
                case Nil => Nil
              }).reverse.mkString("/")
          }
        } else to
      }.getOrElse(to)).get

    val temp = node.attrs.get("base")
      .map(_ => ctx + ("base" -> evaled))
      .getOrElse(ctx)

    val u = url(evaled)
    val theUrl = node.attrs.get("secure").map(_ => u.secure).getOrElse(u)

    for {
      r <- cl(theUrl OK temp)
      lines <- parent.instructions(cl, node.children, r)
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
  import css.query.default._

  def apply(node: LmxmlNode, cl: PuppetClient, ctx: Context) = parent => {
    val byCss = node.attrs.get("by-css").getOrElse("*")
    val results = ctx.response.map(as.TagSoup).map(
      _ ? byCss getOrElse (xml.NodeSeq.Empty)
    )

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
    val (key, attr) = stripAttrs(node.attrs, "set")
    val value = ctx.get[xml.NodeSeq]("find-results").map(n => (n \ attr).text)

    parent.instructions(cl, node.children, ctx + (key -> value.getOrElse("")))
  }
}

/**
 * Iterates over searched results
 *
 * each @value="@href": begins iteration on href attr of node
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
          val (k, attr) = stripAttrs(node.attrs, "each")
          val v = (xs.head \ attr).text
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
