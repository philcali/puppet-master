package com.github.philcali.puppet
package actions
package test

import org.scalatest.FlatSpec
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.ShouldMatchers

import unfiltered.netty._
import unfiltered.request._
import unfiltered.response._

import java.io.File

trait WebInteractions extends FlatSpec with ShouldMatchers with BeforeAndAfterAll {
  implicit def stringToAction(source: String) = ActionWrap(source)

  case class ActionWrap(text: String) {
    def action =
      ActionContext(convertLmxml(text), client, Context(Map()))
    def in (context: Context) = action.copy(ctx = context)
  }

  // Built for you
  lazy val port = unfiltered.util.Port.any
  lazy val url = "http://localhost:%d/" format port
  lazy val http = (Http.local(port).chunked() /: plans)(_ makePlan _)
  lazy val client = dispatch.Http
  lazy val configFile = new File("%s.json" format (this.getClass.getSimpleName))

  def plans: List[cycle.Plan]

  def convertLmxml(source: String) =
    lmxml.DefaultLmxmlParser.parseNodes(source)
         .headOption.map(_.asInstanceOf[lmxml.LmxmlNode]).get

  def configFileContents =
  """
{
  "username": "philcali",
  "password": "********"
}
  """

  override def beforeAll(configMap: Map[String, Any]) {
    // Kick off server
    http.start()

    val writer = new java.io.FileWriter(configFile)
    writer.write(configFileContents)
    writer.close()
  }

  override def afterAll(configMap: Map[String, Any]) {
    // Cleanup server and client
    http.stop()
    client.shutdown()
    configFile.delete()
  }
}
