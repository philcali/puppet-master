package com.github.philcali.puppet

import java.io.{ File, FileOutputStream }
import java.net.URLEncoder
import java.net.URI

import dispatch._

import lmxml.{ LmxmlNode, TextNode, ParsedNode, SinglePass }

object SafeUrl {
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

  def apply(node: LmxmlNode, ctx: Context) = {
    val evaled = node.attrs.get("to")
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
      }.getOrElse(to))
    val theUrl = url(evaled.get)
    val cookied = (theUrl /: ctx.cookies.values)(_ addCookie _)
    if (ctx.data.contains("secure")) cookied.secure else cookied
  }
}

