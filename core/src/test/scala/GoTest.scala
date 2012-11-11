package com.github.philcali.puppet
package actions
package test

import unfiltered.netty._
import unfiltered.request._
import unfiltered.response._

import unfiltered.netty.request._

class GoTest extends WebInteractions {
  object Username extends StringHeader("username")
  object Password extends StringHeader("password")

  // Separate plans into multiple classes to avoid java limit
  def plans = List(cycle.Planify {
    case GET(Path("/")) => ResponseString("Get!")
    case POST(Path("/")) => ResponseString("Post!")
    case req @ PUT(Path("/")) => ResponseString(Body string req)
  }, cycle.Planify {
    case GET(Path("/ask")) & QueryParams(params) =>
      ResponseString(("Got -" /: params)({
        case (in, (k, v)) => in + " %s (%s)" format (k, v.mkString(","))
      }))
    case GET(Path("/headers")) & Username(user) & Password(pass) =>
      ResponseString("Headers %s and %s" format (user, pass))
  }, cycle.Planify {
    case POST(Path("/upload") & MultiPart(req)) =>
      val memory = MultiPartParams.Memory(req)
      memory.files("file") match {
        case Seq((file: MemoryFileWrapper), _*) =>
          ResponseString("Received file %s with contents %s" format (
            file.name, new String(file.bytes, "UTF-8")
          ))
        case _ =>
          ResponseString("Nada")
      }
  })

  "GoAction" should "handle GET requests gracefully" in {
    val action = """go @to="%s" @base""".format(url).action

    val rtn = GoAction perform action

    rtn.ctx.data should contain key "base"
    rtn.ctx.data("base") should be === url
    rtn.ctx.data should contain key "response"
    rtn.ctx.response.map(dispatch.as.String) should be === Some("Get!")
    rtn.node should be === action.node
    rtn.log should be === Some("GET [200] - %s" format url)
  }

  it should "handle POST requests gracefully" in {
    val action = """go @to="%s" @type="POST" """.format(url).action

    val rtn = GoAction perform action

    rtn.ctx.data should contain key "response"
    rtn.ctx.response.map(dispatch.as.String) should be === Some("Post!")
    rtn.node should be === action.node
    rtn.log should be === Some("POST [200] - %s" format url)
  }

  it should "handle PUT requests gracefully" in {
    val source= """go @to="%s" @type="PUT" @value="%s" """.format(url, configFile.getName)

    val rtn = GoAction perform (source action)

    rtn.node.attrs("value") should be === configFile.getName
    rtn.ctx.response.map(dispatch.as.String).get should be === configFileContents
    rtn.log should be === Some("PUT [200] - %s" format url)
  }

  it should "handle request headers with a json file" in {
    val source = """go @to="%sheaders" @req-headers="%s" """.format(url, configFile.getName)

    val rtn = GoAction perform (source action)

    rtn.ctx.response.map(dispatch.as.String) should be ===
      Some("Headers philcali and ********")
  }

  it should "handle query parameters gracefully" in {
    val source = """go @to="%sask" @who="Philip" @what="age" """.format(url)

    val rtn = GoAction perform (source action)

    rtn.ctx.data should contain key "response"
    rtn.ctx.response.map(dispatch.as.String) should be ===
      Some("Got - what (age) who (Philip)")
  }

  it should "handle context variables gracefully" in {
    val source = """go @to="%sask" @who="@who" @what="@what" """.format(url)

    val variables = Map("who" -> "Jimbo", "what" -> "lastname")

    val rtn = GoAction perform (source in Context(variables))

    rtn.log should be ===
      Some("GET [200] - %sask?who=Jimbo&what=lastname" format url)
    rtn.ctx.response.map(dispatch.as.String) should be ===
      Some("Got - what (lastname) who (Jimbo)")
  }

  it should "load parameters from json file" in {
    val source = """go @to="%sask" @file-params="%s" """ format (url, configFile.getName)

    val rtn = GoAction perform (source action)

    rtn.ctx.response.map(dispatch.as.String) should be ===
      Some("Got - password (********) username (philcali)")
  }

  it should "upload a file" in {
    val source = """go @to="%supload" @type="POST" @file-upload-file="%s" """.format(url, configFile.getName)

    val rtn = GoAction perform (source action)

    rtn.log should be === Some("POST [200] - %supload" format url)
    rtn.ctx.response.map(dispatch.as.String) should be ===
      Some("Received file %s with contents %s" format (
        configFile.getName, configFileContents
      ))
  }
}
