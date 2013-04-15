package com.codeforz.sclicks.browser

import scala.concurrent._
import org.apache.http.impl.client.DefaultHttpClient
import org.apache.http.client.methods.HttpGet
import org.cyberneko.html.parsers.DOMParser
import org.xml.sax.InputSource
import org.w3c.dom.{NodeList, Document}

class RequestFailureException(val status:Int, val message:String) extends Exception

implicit class RichNodeList(nodeList:NodeList) extends AnyVal{

}
/**
 *
 */
class Browser {
  lazy val httpClient = new DefaultHttpClient()

  def visit(url:String):Future[WebPage]={
    future{
      val response = httpClient.execute(new HttpGet(url))
      response.getStatusLine.getStatusCode match{
        case x if(x>=200 && x<400) =>
          val htmlParser = new DOMParser
          htmlParser.parse(new InputSource(response.getEntity.getContent))
          WebPage(htmlParser.getDocument)
        case _ =>
          throw new RequestFailureException(response.getStatusLine.getStatusCode, response.getStatusLine.getReasonPhrase)
      }
    }
  }
}

case class WebPage(document:Document){
  def title = document.getElementsByTagName("title")
}

