package com.github.philcali
package puppet
package app

import utils.PropertiesLoader

import lmxml.{ PlainLmxmlFactory, FileLoading }
import lmxml.transforms.{ Transform, Value }
import lmxml.transforms.json.JSTransform

import java.io.File

object Lmxml extends PlainLmxmlFactory with FileLoading

object Main {
  val DataFile = """\s+-d\s+(.*\.json$)\s+""".r

  def printHelp = {
    println("""
  puppet v0.1.0, Copyright Philip Cali 2012

  puppet [-d transform.json] instructions.lmxml [output.log]

  ex:
    puppet instructions.lmxml > program.out
    puppet instructions.lmxml output.log
    puppet -d dynamic.json instructions.lmxml output.log
  """)
  }

  def isValid(file: String) = new File(file).exists

  def main(args: Array[String]) {
    val str = args.mkString(" ")

    val data = DataFile.findFirstMatchIn(str).map(_.group(1)).filter(isValid)

    DataFile.replaceAllIn(str, "").split(" ") match {
      case Array(file, out) if isValid(file) => perform(data, Some(out), file)
      case Array(file) if isValid(file) => perform(data, None, file)
      case _ => printHelp
    }
  }

  def perform(data: Option[String], out: Option[String], lmxmlFile: String) {
    val loader = new PropertiesLoader
    val systemProps = loader.convertProperties(System.getProperties).map({
      case (k, v) => k.replaceAll("\\.", "-") -> Value(v)
    })

    val systemDefaults = Transform(systemProps.toSeq : _*)

    val trans = data.map(io.Source.fromFile)
      .map(_.getLines.mkString("\n"))
      .map(JSTransform().parse).map(_ + systemDefaults)
      .getOrElse(systemDefaults)

    val logs = Lmxml.fromFile(lmxmlFile)(trans andThen Instructions.Default)

    val output = out.map(new java.io.FileWriter(_))

    for {
      lines <- logs
    } {
      lines.foreach(l => output.map(_.write(l + "\n")).getOrElse(println(l)))
    }

    output.map(_.close)
  }
}

class Main extends xsbti.AppMain {
  case class Exit(code: Int) extends xsbti.Exit

  def run(config: xsbti.AppConfiguration) = {
    try {
      Main.main(config.arguments)
      Exit(0)
    } catch {
      case e =>
        println("[FAILED]: %s" format e.getMessage)
        Exit(1)
    }
  }
}
