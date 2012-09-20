package com.github.philcali
package puppet
package utils

import java.io.{ File, FileInputStream }
import java.util.Properties

import util.parsing.json._

trait ParamLoader {
  def validate(source: String): Boolean
  def convert(source: String): Map[String, String]

  def load(source: String) =
    if (validate(source)) Some(convert(source)) else None
}

trait FileLoader extends ParamLoader {
  def validate(source: String) = new File(source).exists
}

class JsonLoader extends FileLoader {
  def convert(source: String) = {
    import scala.io.Source.{fromFile => open}
    JSON.parseRaw(open(source).getLines.mkString("\n")).map {
      case JSONArray(ls) =>
        Map("params" -> ls.mkString(","))
      case JSONObject(obj) =>
        obj.map { case (k, v) => k -> v.toString }
    }.getOrElse(Map.empty)
  }
}

class PropertiesLoader extends FileLoader {
  import collection.JavaConversions.asScalaSet

  def convertProperties(p: Properties) = {
    p.entrySet.map(e => e.getKey.toString -> e.getValue.toString).toMap
  }

  def convert(source: String) = {
    val in = new FileInputStream(new File(source))
    try {
      val p = new Properties()
      p.load(in)
      convertProperties(p)
    } catch {
      case _ => Map.empty
    } finally {
      in.close()
    }
  }
}

object Params extends FileLoader {
  lazy val properties = new PropertiesLoader
  lazy val json = new JsonLoader

  def convert(source: String) = if (source.endsWith("json"))
    json.convert(source) else properties.convert(source)
}
