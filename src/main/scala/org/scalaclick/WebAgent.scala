package org.scalaclick

import com.gargoylesoftware.htmlunit.html._
import org.apache.log4j.Logger

import org.apache.commons.io.FileUtils
import java.io.File
import com.gargoylesoftware.htmlunit._
import scala.xml.XML

/**
 * Simple HTMLUnit wrapper for basic UI interactions: type and click
 */
object WebAgent {
  private[WebAgent] val logger = Logger.getLogger(classOf[WebAgent])

  def defaultClient = {
    val webClient = new WebClient(BrowserVersion.FIREFOX_3_6)
    webClient.setThrowExceptionOnScriptError(false)
    webClient.setAjaxController(new NicelyResynchronizingAjaxController)

    if (sys.props("http.proxyHost") != null) {
      webClient.setProxyConfig(new ProxyConfig(sys.props("http.proxyHost"), sys.props("http.proxyPort").toInt))
    }
    webClient
  }

  val defaultWaitBefore = 2000

  def open(url: String) = {
    val page: HtmlPage = defaultClient.getPage(url)
    logger.debug("Page :" + url + "==================\n" + page.asText())
    new WebAgent(page)
  }

  def apply(url: String) = open(url)
}

import WebAgent._

class WebAgent private(page: HtmlPage) {
  import collection.JavaConversions._
  import ElementFinder._

  def title = page.getTitleText

  def click(selector: String, wait:Long=0) = {
    val elem = element[HtmlElement](selector)
    val page = elem.click[HtmlPage]()
    val count = page.getWebClient.waitForBackgroundJavaScript(wait)
    if (count > 0) {
      logger.warn(count + " background scripts are still running")
    }
    if (page == this.page) this else {
      logger.debug("Click on " + elem.asXml() + ":============\n" + page.asXml())
      new WebAgent(page)
    }
  }

  def select(selector:String) = elements[HtmlElement](selector).map(new MatchedElement(_))

  def typeKeys(target:String, str:String){
    element[HtmlElement](target).`type`(str)
  }

  def asXml(selector:String) = XML.load(element[HtmlElement](selector).asXml())

  def fill(formSelector:String, values:Map[String,String]){
    values.foreach{case (k,v) => val inputs = element[HtmlForm](formSelector).getInputsByName(k)
      if(inputs.isEmpty) sys.error("form element not found:" + k)
      inputs.foreach(e=>if(e.isInstanceOf[HtmlSelect]) e.asInstanceOf[HtmlSelect].setSelectedAttribute(v, true)
        else if (e.isInstanceOf[HtmlRadioButtonInput]) { val radio = e.asInstanceOf[HtmlRadioButtonInput]
          if (radio.getValueAttribute == v) radio.setChecked(true)
        }else if (e.isInstanceOf[HtmlCheckBoxInput]){val check = e.asInstanceOf[HtmlCheckBoxInput]
          check.setChecked(check.getValueAttribute==v)
        }else if (e.isInstanceOf[HtmlInput]) e.asInstanceOf[HtmlInput].setValueAttribute(v)
        else if (e.isInstanceOf[HtmlTextArea]) e.asInstanceOf[HtmlTextArea].setText(v)
        else sys.error("unsupported element for form fill:"  + k)
      )
    }
  }

  def asText = try {
    page.asText()
  } catch {
    case e => logger.error("Failed extracting page text", e); "--FAILED TO EXTRACT PAGE TEXT--"
  }

  /**
   * Extracts values of the elements matched by the given selector. Only HTML input and option elements are supported
   * @param selector
   * @return
   */
  def values(selector:String):Seq[String] = elements[HtmlElement](selector).map(e=>{
    if (e.isInstanceOf[HtmlOption]) e.asInstanceOf[HtmlOption].getValueAttribute
    else if (e.isInstanceOf[HtmlInput]) e.asInstanceOf[HtmlInput].getValueAttribute
    else if (e.isInstanceOf[HtmlTextArea]) e.asInstanceOf[HtmlTextArea].getText
    else  sys.error("Element of type " + e.getClass + " does not support values")
  })

  def attributes(selector:String, attr:String) = elements[HtmlElement](selector).map(e=>e.getAttribute(attr))

  def attr(selector:String, attr:String) = element[HtmlElement](selector).getAttribute(attr)

  def text(selector:String) = elements[HtmlElement](selector).map(e=>e.getTextContent)

  private def element[T <: HtmlElement](selector: String): T = find(selector) match {
    case Some(e) => e
    case _ => val tmpfile = File.createTempFile("page", ".html")
    FileUtils.writeStringToFile(tmpfile, page.asXml())
    sys.error("Element not found :" + selector + " (see page html dump in " + tmpfile.getAbsolutePath + ")");
  }

  private def elements[T <: HtmlElement](selector: String): Seq[T] = findAll(page.getEnclosingWindow.getTopWindow.
    getEnclosedPage.asInstanceOf[HtmlPage].getDocumentElement, selector)

  private def find[T <: HtmlElement](selector: String) = findFirst[T](page.getEnclosingWindow.getTopWindow.
    getEnclosedPage.asInstanceOf[HtmlPage].getDocumentElement, selector)

  def getAsStream(path: String) = {
    val response = page.getWebClient.loadWebResponse(new WebRequest(page.getFullyQualifiedUrl(path)))
    if (response.getStatusCode != 200) {
      sys.error("No Ok response received for " + path + "." + response.getStatusMessage)
    }
    else response.getContentAsStream
  }

}


class MatchedElement(elem:HtmlElement){
  def text = elem.getTextContent
  def xml = elem.asXml()
  def attr(name:String) = elem.getAttribute(name)
  def value = if(elem.isInstanceOf[HtmlInput]) elem.asInstanceOf[HtmlInput].getValueAttribute
    else if (elem.isInstanceOf[HtmlOption]) elem.asInstanceOf[HtmlOption].getValueAttribute
    else if (elem.isInstanceOf[HtmlTextArea]) elem.asInstanceOf[HtmlTextArea].getText
}
