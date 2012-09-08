package com.github.philcali.puppet

import dispatch._

import lmxml.{ LmxmlNode, TextNode, ParsedNode, SinglePass }

import com.ning.http.client.{
  AsyncHttpClientConfig => Config,
  AsyncHttpClient,
  Response
}

import java.io.{ File, FileOutputStream }

case class PuppetClient(config: Config) extends Executor {
  val client = new AsyncHttpClient(config)

  val timeout = Duration.Zero
}

object PuppetConfig {
  import collection.JavaConversions.asScalaSet

  val defaultConfig = default.build

  def default = new Config.Builder

  def loadMap(options: Map[String, String], config: Config.Builder = default) = {
      config.setFollowRedirects(
        options.get("follow-redirects").map(_.toBoolean)
               .getOrElse(defaultConfig.isRedirectEnabled)
      ).setAllowPoolingConnection(
        options.get("keep-alive").map(_.toBoolean)
               .getOrElse(defaultConfig.getAllowPoolingConnection)
      ).setUserAgent(
        options.get("user-agent").getOrElse(defaultConfig.getUserAgent)
      ).setMaximumConnectionsTotal(
        options.get("max-connections").map(_.toInt)
               .getOrElse(defaultConfig.getMaxTotalConnections)
      ).setCompressionEnabled(
        options.get("compression-enabled").map(_.toBoolean)
               .getOrElse(defaultConfig.isCompressionEnabled)
      ).setRequestCompressionLevel(
        options.get("request-compression-level").map(_.toInt)
               .getOrElse(defaultConfig.getRequestCompressionLevel)
      ).setIdleConnectionInPoolTimeoutInMs(
        options.get("idle-connection-timeout").map(_.toInt)
               .getOrElse(defaultConfig.getIdleConnectionInPoolTimeoutInMs)
      ).setRequestTimeoutInMs(
        options.get("request-timeout").map(_.toInt)
               .getOrElse(defaultConfig.getRequestTimeoutInMs)
      )
  }

  def fromFile(loc: String, origin: Config.Builder = default) =
    loadFile(loc).map(loadMap(_, origin)).getOrElse(origin)

  def fromLmxml(nodes: Seq[ParsedNode], origin: Config.Builder = default) =
    flattenConfigNodes(nodes).map(loadMap(_, origin)).getOrElse(origin)

  def flattenConfigNodes(nodes: Seq[ParsedNode]) = {
    val toText = (node: Option[ParsedNode]) =>
      node.filter(_.isInstanceOf[TextNode])
          .map(_.asInstanceOf[TextNode])
          .map(_.contents).getOrElse("")

    nodes.headOption.filter(_.name == "config").map { node =>
      Map(node.children.foldLeft(List[(String, String)]())({
        case (in, n) => (n.name, toText(n.children.headOption)) :: in
      }):_*)
    }
  }

  private def loadFile(loc: String) = {
    val file = new File(loc)
    if (file.exists) {
      val in = new java.io.FileInputStream(file)
      val p = new java.util.Properties()
      p.load(in)
      in.close()
      Some(p.entrySet.map(e => e.getKey.toString -> e.getValue.toString).toMap)
    } else {
      None
    }
  }
}

case class Context(data: Map[String, Any]) extends (Response => Context) {
  def response = get[Response]("response")

  def get[A](key: String) = data.get(key).map(_.asInstanceOf[A])

  def unwrap(kv: (String, Option[_])) =
    kv._2.map(p => this + (kv._1 -> p)).getOrElse(this)

  def + (kv: (String, Any)) = Context(data + kv)

  def apply(r: Response) = this + ("response" -> r)

  override def toString() = "Context(data = %s)" format data.toString
}
