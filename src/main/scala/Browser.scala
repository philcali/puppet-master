package com.github.philcali.puppet

import lmxml.{ LmxmlNode, TextNode, ParsedNode, SinglePass }
import lmxml.transforms.{ Foreach, If, Else, Value }

import css.query.default._
import dispatch._

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
}

case class Instructions(logger: List[String] => Unit)
  extends SinglePass[Promise[PuppetClient]] {

  type Nodes = Seq[ParsedNode]

  val logResponse = "[%d] - %s"

  def single(node: ParsedNode) = node match {
    case LmxmlNode(name, attrs, children) if name == "browser" =>
      val config = attrs.get("file")
                        .map(PuppetConfig.fromFile(_))
                        .getOrElse(PuppetConfig.default)

      val browser = PuppetClient(PuppetConfig.fromLmxml(children, config).build)
      val ctx = Context(Map.empty)
      for (lines <- instructions(browser, children.tail, ctx)) yield {
        logger(lines)
        browser
      }
  }

  def pump(in: java.io.InputStream, out: java.io.OutputStream): Unit = {
    val buffer = new Array[Byte](1024)
    in.read(buffer) match {
      case n if n >= 0 => out.write(buffer, 0, n); pump(in, out)
      case _ => out.close()
    }
  }

  def stripAttrs(attrs: Map[String, String], default: String) = {
    val key = attrs.get("name").getOrElse(default)
    val attr = attrs.get("value").filter(_.startsWith("@"))
    (key -> attr.getOrElse("@value"))
  }

  def instructions(cl: PuppetClient, nodes: Nodes, ctx: Context): Promise[List[String]] = nodes match {
    case node :: ns if node.name == "instructions" =>
      for {
        first <- instructions(cl, node.children, ctx)
        second <- instructions(cl, ns, ctx)
      } yield (first ::: second)
    case LmxmlNode(name, attrs, children) :: ns => name match {
      case "go" if attrs.contains("to") =>
        val evaled = attrs.get("to")
          .map(to => ctx.get(to).getOrElse(to))
          .map(to => ctx.response.map(_.getUri).map { uri =>
            if (!to.contains("://")) {
              val str = ctx.get("base").getOrElse(uri.toURL.toString)
              str.stripSuffix("/") + "/" + to.stripPrefix("/")
            } else to
          }.getOrElse(to)).get

        val temp = attrs.get("base")
          .map(_ => ctx + ("base" -> evaled))
          .getOrElse(ctx)

        val u = url(evaled)
        val theUrl = attrs.get("secure").map(_ => u.secure).getOrElse(u)

        for {
          r <- cl(theUrl OK temp)
          lines <- instructions(cl, children, r)
        } yield {
          val res = r.response.get
          logResponse.format (res.getStatusCode, res.getUri.toURL) :: lines
        }
      case "find" =>
        val regex = attrs.get("by-regex").map(_.r).getOrElse(""".*""".r)
        val results = attrs.get("node").map { elem =>
          val flat = (n: xml.Node) =>
            attrs.get("by-attr").map(a => n \ a text).getOrElse(n.text)
          ctx.response.map(as.TagSoup).map {
            _ \\ elem filter (n => regex.findFirstMatchIn(flat(n)).isDefined)
          }.get
        }
        instructions(cl, children, ctx unwrap ("find-results" -> results))
      case "set" if attrs.contains("value") =>
        val (key, attr) = stripAttrs(attrs, name)
        val value = ctx.get[xml.NodeSeq]("find-results")
                       .map(n => (n \\ attr).text)
        instructions(cl, children, ctx + (key -> value.getOrElse("")))
      case "each" if attrs.contains("value") =>
        ctx.get[xml.NodeSeq]("find-results").map({
          case xs if xs.headOption.isDefined =>
            val (k, attr) = stripAttrs(attrs, name)
            val v = (xs.head \\ attr).text
            for {
              s1 <- instructions(cl, children, ctx + (k -> v))
              s2 <- instructions(cl, nodes, ctx + ("find-results" -> xs.tail))
            } yield (s1 ::: s2)
          case _ => Promise(Nil)
        }).getOrElse(Promise(Nil))
      case "println" =>
        ctx.response.map(as.String).foreach(println)
        instructions(cl, children, ctx)
      case "download" =>
        val dest = attrs.get("to").map(new File(_)).getOrElse(new File("."))
        if (!dest.exists) dest.mkdir
        ctx.response.map { res =>
          val str = res.getUri.toURL.getFile
          val file = new File(dest, str.split("/").takeRight(1).head)
          pump(res.getResponseBodyAsStream, new FileOutputStream(file))
        }
        instructions(cl, children, ctx)
    }
    case node :: ns => instructions(cl, ns, ctx)
    case Nil => Promise(Nil)
  }
}
