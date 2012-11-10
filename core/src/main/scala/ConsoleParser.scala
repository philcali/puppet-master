package com.github.philcali.puppet

import util.parsing.combinator.RegexParsers
import sys.process._
import java.io.File

trait ConsoleParsers extends RegexParsers {
  type Builder = (ProcessBuilder => ProcessBuilder)

  class ConsoleOut extends ProcessLogger {
    val outLines = collection.mutable.ListBuffer[String]()
    val errLines = collection.mutable.ListBuffer[String]()

    def outputLines = outLines.toList
    def output = outLines.mkString("\n")

    def errorLines = errLines.toList
    def error = errLines.mkString("\n")

    def out(s: => String): Unit = outLines += s
    def err(s: => String): Unit = errLines += s
    def buffer[T](f: => T): T = f
  }

  def command = """[^\|><&;]+""".r

  def process = command ^^ (Process(_))

  def redirectedIn: Parser[Builder] = "<" ~> process ^^ { p => _ #< p }

  def redirectedOut: Parser[Builder] = ">" ~> command ^^ { file =>
    _ #> new File(file)
  }

  def piped: Parser[Builder] = "|" ~> process ^^ { p => _ #| p }

  def conditional: Parser[Builder] = "&&" ~> process ^^ { p => _ #&& p }

  def separate: Parser[Builder] = ";" ~> process ^^ { p => _ ### p }

  def breaks = (redirectedOut | redirectedIn | piped | conditional | separate)

  def line = process ~ rep(breaks) ^^ {
    case cmd ~ bs => (cmd /: bs)({ case (c, b) => b(c) })
  }

  def execute(cmds: String) = parseAll(line, cmds) match {
    case Success(p, i) =>
      val console = new ConsoleOut
      val code = p ! console
      code -> console
    case e: NoSuccess =>
      val console = new ConsoleOut
      console err ("ERROR: %s" format e.msg)
      1 -> console
  }
}
