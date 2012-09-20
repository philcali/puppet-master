package dispatch.as

import com.ning.http.client.{ Response => Original }
import org.xml.sax.InputSource

object TagSoup extends (Original => scala.xml.Node) {
  val adapter = new scala.xml.parsing.NoBindingFactoryAdapter
  val fac = new org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl

  def apply(r: Original) = {
    adapter.loadXML(new InputSource(r.getResponseBodyAsStream), fac.newSAXParser)
  }
}
