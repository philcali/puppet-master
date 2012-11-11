package com.github.philcali.puppet

import utils.Params
import lmxml.{ LmxmlNode, TextNode, ParsedNode, SinglePass }

import css.query.default._

import util.parsing.combinator.RegexParsers
import tools.jline.console.completer.FileNameCompleter

object NodeParams extends ParamParsers {
  override def skipWhitespace = false

  def option = fromOption
}

object NodeParamsEmbeded extends ParamParsers {
  override def everything = """[^\[@\]]+""".r

  override def fromOption = (super.fromOption | fromValue)

  def fromValue: Parser[Yank] = "@" ~> ident ^^ { key =>
    (value, context) => context.get[Any](key).map(_.toString).getOrElse(value)
  }

  def encagedOption: Parser[Yank] = "[" ~> fromOption <~ "]"

  def embededOption: Parser[Yank] = opt(everything) ~ encagedOption ^^ {
    case (optContent ~ fun) => (value, context) => {
      val replacement = fun(value, context)
      optContent.map(_ + replacement).getOrElse(replacement)
    }
  }

  def reverseEmbed: Parser[Yank] = opt(encagedOption) ~ everything ^^ {
    case optFun ~ content => (value, context) => {
      optFun.map(_.apply(value, context)).getOrElse("") + content
    }
  }

  def option: Parser[Yank] = rep1(embededOption | reverseEmbed) ^^ { options =>
    (value, context) => options.map(_.apply(value, context)).mkString(" ")
  }
}

trait ParamParsers extends RegexParsers {

  protected val reservedKeys = List("to", "base", "secure", "type")

  type Yank = (String, Context) => String

  val reader = {
    val r = new tools.jline.console.ConsoleReader()
    r.addCompleter(new FileNameCompleter())
    r
  }

  val ident = """[a-zA-Z0-9_\-]+""".r

  def everything = """[^@]+""".r

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

  def fromOption = (fromCss | fromPrompt | fromPass)

  def option: Parser[Yank]

  def yank(input: String): Yank = parseAll(option, input) match {
    case Success(fun, _) => fun
    case _ => (value, ctx) =>
      if (value.startsWith("@")) {
        ctx.get[String](value.stripPrefix("@")).map(_.toString).getOrElse(value)
      } else value
  }

  def apply(node: LmxmlNode, ctx: Context) = {
    val submitted = (node.attrs /: reservedKeys)(_ - _)

    (submitted.get("file-params") match {
      case Some(file) if Params.validate(file) =>
        Params.convert(file) ++ (submitted - "file-params")
      case _ => submitted
    }).map {
      case (k, v) => k -> yank(v).apply(v, ctx)
    }
  }
}

