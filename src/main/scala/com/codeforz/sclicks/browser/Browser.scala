package com.codeforz.sclicks.browser

import scala.concurrent._
import org.apache.http.impl.client.DefaultHttpClient
import org.apache.http.client.methods.HttpGet
import org.cyberneko.html.parsers.DOMParser
import org.xml.sax.InputSource
import org.w3c.dom.{Element, NodeList, Document}
import org.apache.http.conn.params.ConnRoutePNames
import org.apache.http.HttpHost
import concurrent.ExecutionContext.Implicits.global
import javax.xml.transform.{OutputKeys, TransformerFactory}
import javax.xml.transform.dom.DOMSource
import java.io.StringWriter
import javax.xml.transform.stream.StreamResult
import com.typesafe.scalalogging.slf4j.LazyLogging

class RequestFailureException(val status:Int, val message:String) extends Exception

object Browser{
  implicit class RichNodeList(val nodeList:NodeList) extends AnyVal{
    def headOption = if(nodeList.getLength>0) Some(nodeList.item(0).asInstanceOf[Element]) else None
    def seq = Stream.range(0, nodeList.getLength-1).map(nodeList.item(_).asInstanceOf[Element])
  }

}
import Browser._
/**
 *
 */
class Browser extends LazyLogging{
  lazy val httpClient = {
    val client = new DefaultHttpClient()

    for{host<-sys.props.get("http.proxyHost")
      port<-sys.props.get("http.proxyPort")
    }{
      client.getParams.setParameter(ConnRoutePNames.DEFAULT_PROXY, new HttpHost(host, port.toInt))
    }

//    for{host<-sys.props.get("https.proxyHost")
//      port<-sys.props.get("https.proxyPort")
//    }{
//      client.getParams.setParameter(ConnRoutePNames.DEFAULT_PROXY, new HttpHost(host, port.toInt, "https"))
//    }

    client
  }

  def visit(url:String):Future[WebPage]= future{get(url)}

  def get(url: String): WebPage = {
    val response = httpClient.execute(new HttpGet(url))
    response.getStatusLine.getStatusCode match {
      case x if (x >= 200 && x < 400) =>
        val htmlParser = new DOMParser
        htmlParser.parse(new InputSource(response.getEntity.getContent))
        WebPage(htmlParser.getDocument)
      case _ =>
        throw new RequestFailureException(response.getStatusLine.getStatusCode, response.getStatusLine.getReasonPhrase)
    }
  }
}

case class WebPage(document:Document) extends LazyLogging{
  document.getElementsByTagName("SCRIPT").seq foreach{s=>
    print("Executing script " + s.getAttribute("src"))
  }

  def title = document.getElementsByTagName("title").headOption.map(_.getTextContent).getOrElse("")
  def bodyAsText = document.getElementsByTagName("body").headOption.map(_.getTextContent).getOrElse("")
  def asXml = {
    val result = new StreamResult(new StringWriter())
    val transformer = TransformerFactory.newInstance().newTransformer()
    transformer.setOutputProperty(OutputKeys.INDENT, "yes")
    transformer.transform(new DOMSource(document), result)
    result.getWriter.toString
  }
}

