package com.codeforz.scalaclick

import com.gargoylesoftware.htmlunit.html._

import org.apache.commons.io.FileUtils
import java.io.File
import com.gargoylesoftware.htmlunit._
import java.util.logging.Logger
import util.WebConnectionWrapper


trait ConnectionListener{
  def requesting(req:WebRequest)
  def responded(req:WebRequest, resp:WebResponse)
}
/**
 * Simple HTMLUnit wrapper for basic UI interactions: type and click
 */
object WebPage {
  private[WebPage] val logger = Logger.getLogger(classOf[WebPage].getName)

  val CHROME_20 = new BrowserVersion("CHROME", "5.0 (Windows NT 6.2)", "Mozilla/5.0 (Windows NT 6.2) AppleWebKit/536.6 (KHTML, like Gecko) Chrome/20.0.1090.0 Safari/536.6", 20)
  val FIREFOX_11 = new BrowserVersion("Mozilla", "5.0 (Windows NT 6.1; rv:12.0)", "Mozilla/5.0 (Windows NT 6.1; rv:12.0) Gecko/20120403211507 Firefox/12.0", 12)

  def defaultClient(listeners:Seq[ConnectionListener])(implicit browser: BrowserVersion = BrowserVersion.FIREFOX_3_6) = {
    val webClient = new WebClient(browser)
    webClient.setThrowExceptionOnScriptError(false)
    webClient.setAjaxController(new NicelyResynchronizingAjaxController)

    if (sys.props("http.proxyHost") != null) {
      webClient.setProxyConfig(new ProxyConfig(sys.props("http.proxyHost"), sys.props("http.proxyPort").toInt))
    }
    if (listeners.nonEmpty){
      //wrap connection
      webClient.setWebConnection(new WebConnectionWrapper(webClient.getWebConnection){
        override def getResponse(request: WebRequest) = {
          listeners.foreach(_.requesting(request))
          val response = super.getResponse(request)
          listeners.foreach(_.responded(request, response))
          response
        }
      })
    }
    webClient
  }

  /**
   * Opens the specified URL as a WebPage
   * @param url
   * @param browser the HtmlUnit browser version implementation. Defaults to Firefox-3.6 with proxy, ajax support and ignore JS errors
   * @return
   */
  def open(url: String, listeners:ConnectionListener*)(implicit browser: BrowserVersion = BrowserVersion.FIREFOX_3_6) = {
    val page: HtmlPage = defaultClient(listeners)(browser).getPage(url)
    logger.fine("Page :" + url + "==================\n" + page.asText())
    new WebPage(page)
  }

  /**
   * Create a new WebPage from the given URL
   * @param url
   * @return
   */
  def apply(url: String) = open(url)
}

import WebPage._

/**
 * A simple HtmlPage wrapper that allows usual page interactions (type, click) as well as HTML element queries
 * @param page
 */
class WebPage private(private var page: HtmlPage) {

  import collection.JavaConversions._
  import ElementFinder._

  private implicit val thisPage:WebPage = this

  /**
   * Page title
   * @return
   */
  def title = page.getTitleText

  /**
   * Simulates a click on the element matching the specified selector. The click action may result in a new page loading
   * in which case page field will be update to the new page.
   *
   * @param selector An element select (see Selectors)
   * @param wait Time in millisecs to wait for javascript execution after page load
   * @return this same instance
   */
  def click(selector: String, wait: Long = 2000) = {
    val elem = element[HtmlElement](selector)
    doClick(elem, wait)
  }

  private[scalaclick] def doClick(elem: HtmlElement, wait: Long): WebPage = {
    val previous = page
    page = elem.click[HtmlPage]()
    val count = page.getWebClient.waitForBackgroundJavaScript(0)
    if (count > 0) {
      logger.warning(count + " background scripts are still running")
      page.getWebClient.waitForBackgroundJavaScript(wait)
    }
    if (previous != this.page) {
      logger.fine("click on " + elem.asXml() + ":============\n" + page.getTitleText)
    }
    this
  }

  def mouseDown(selector: String, wait: Long = 2000) = mouseAction(selector, "mousedown", wait)

  private def mouseAction(selector:String, action:String, wait:Long=2000)={
    val elem = element[HtmlElement](selector)
    val previous = page
    page = fireEvent(elem, action)
    val count = page.getWebClient.waitForBackgroundJavaScript(0)
    if (count > 0) {
      logger.warning(count + " background scripts are still running")
      page.getWebClient.waitForBackgroundJavaScript(wait)
    }
    if (previous != this.page) {
      logger.fine(action + " on " + elem.asXml() + ":============\n" + page.getTitleText)
    }
    this
  }

  private[scalaclick] def fireEvent(elem:HtmlElement, event:String)={
    val result = elem.fireEvent(event)
    if(result==null) page
    else result.getNewPage.asInstanceOf[HtmlPage]
  }

  /**
   * Returns all elements matching the given selector
   * @param selector
   * @return
   */
  def all(selector: String) = elements[HtmlElement](selector).map(new MatchedElement(_))

  /**
   * Returns first element matching the given selector
   * @param selector
   * @return
   */
  def first(selector: String) = new MatchedElement(element[HtmlElement](selector))

  /**
   * Finds the element matching the given selector
   * @param selector
   * @return
   */
  def find(selector: String) = findElement[HtmlElement](selector).map(new MatchedElement(_))

  /**
   * Types the string into the element matching the target selector
   * @param target
   * @param str
   */
  def typeString(target: String, str: String) {
    element[HtmlElement](target).`type`(str)
  }

  /**
   * Returns the XML representation of the element matching the selector
   * @param selector
   * @return
   */
  def asXml(selector: String) = element[HtmlElement](selector).asXml()

  /**
   * Fills the elements of the form matching the given selector, with the provided values
   * @param formSelector
   * @param values
   */
  def fill(formSelector: String, values: Map[String, String]) {
    values.foreach {
      case (k, v) => val inputs = element[HtmlForm](formSelector).getInputsByName(k)
      if (inputs.isEmpty) sys.error("form element not found:" + k)
      inputs.foreach(e => if (e.isInstanceOf[HtmlSelect]) e.asInstanceOf[HtmlSelect].setSelectedAttribute(v, true)
      else if (e.isInstanceOf[HtmlRadioButtonInput]) {
        val radio = e.asInstanceOf[HtmlRadioButtonInput]
        if (radio.getValueAttribute == v) radio.setChecked(true)
      } else if (e.isInstanceOf[HtmlCheckBoxInput]) {
        val check = e.asInstanceOf[HtmlCheckBoxInput]
        check.setChecked(check.getValueAttribute == v)
      } else if (e.isInstanceOf[HtmlInput]) e.asInstanceOf[HtmlInput].setValueAttribute(v)
      else if (e.isInstanceOf[HtmlTextArea]) e.asInstanceOf[HtmlTextArea].setText(v)
      else sys.error("unsupported element for form fill:" + k)
      )
    }
  }

  /**
   * Returns the page text content
   * @return Page text content
   */
  def asText = try {
    page.asText()
  } catch {
    case e => logger.severe("Failed extracting page text " + e); "--FAILED TO EXTRACT PAGE TEXT--"
  }

  def saveTo(file: String) {
    FileUtils.writeStringToFile(new File(file), page.asXml())
  }

  /**
   * Extracts values of the elements matched by the given selector. Only HTML input and option elements are supported
   * @param selector
   * @return
   */
  def values(selector: String): Seq[String] = elements[HtmlElement](selector).map(e => {
    if (e.isInstanceOf[HtmlOption]) e.asInstanceOf[HtmlOption].getValueAttribute
    else if (e.isInstanceOf[HtmlInput]) e.asInstanceOf[HtmlInput].getValueAttribute
    else if (e.isInstanceOf[HtmlTextArea]) e.asInstanceOf[HtmlTextArea].getText
    else sys.error("Element of type " + e.getClass + " does not support values")
  })

  /**
   * Returns the specified attribute value of the element matching the selector
   * @param selector
   * @param attr
   * @return
   */
  def attr(selector: String, attr: String) = element[HtmlElement](selector).getAttribute(attr)

  /**
   * Returns the text content of the element matching the given selector
   * @param selector
   * @return
   */
  def text(selector: String) = elements[HtmlElement](selector).map(e => e.getTextContent)

  /**
   * Downloads the content of the resource referenced by the relative path
   * @param path
   * @return
   */
  def getAsStream(path: String) = {
    val response = page.getWebClient.loadWebResponse(new WebRequest(page.getFullyQualifiedUrl(path)))
    if (response.getStatusCode != 200) {
      sys.error("No Ok response received for " + path + "." + response.getStatusMessage)
    }
    else response.getContentAsStream
  }

  private def element[T <: HtmlElement](selector: String): T = findElement(selector) match {
    case Some(e) => e
    case _ => logger.warning("================Element " + selector + " not found: page dump========================\n" + page.asXml()
      + "\n===========================================================\n")
    sys.error("Element not found :" + selector )
  }

  private def elements[T <: HtmlElement](selector: String): Seq[T] = findAll(page.getEnclosingWindow.getTopWindow.
    getEnclosedPage.asInstanceOf[HtmlPage].getDocumentElement, selector)

  private def findElement[T <: HtmlElement](selector: String) = findFirst[T](page.getEnclosingWindow.getTopWindow.
    getEnclosedPage.asInstanceOf[HtmlPage].getDocumentElement, selector)


}


/**
 * A simple wrapper around HTML Elements to provide common and most used methods
 * @param elem
 */
class MatchedElement(elem: HtmlElement)(implicit page:WebPage) {
  /**
   * Text content of the element
   * @return
   */
  def text = elem.getTextContent

  /**
   * XML representation of the element (including descendants)
   * @return
   */
  def xml = elem.asXml()

  /**
   * Attribute value of the element
   * @param name
   * @return
   */
  def attr(name: String) = elem.getAttribute(name)

  /**
   * Value of the element. Only Form elements are supported i.e html input, option, select(single value), and textarea
   * @return
   */
  def value = if (elem.isInstanceOf[HtmlInput]) elem.asInstanceOf[HtmlInput].getValueAttribute
  else if (elem.isInstanceOf[HtmlOption]) elem.asInstanceOf[HtmlOption].getValueAttribute
  else if (elem.isInstanceOf[HtmlTextArea]) elem.asInstanceOf[HtmlTextArea].getText
  else if (elem.isInstanceOf[HtmlSelect]) elem.asInstanceOf[HtmlSelect].getSelectedOptions.get(0).getValueAttribute
  else sys.error("Value not supported in element " + elem)

  /**
   * Types the string into the element
   * @param str
   */
  def typeString(str: String) {
    val selectable = elem.asInstanceOf[ {def select()}]
    selectable.select()
    elem.`type`(str)
  }

  /**
   * Sets the value of the elements
   * @param newVal
   * @return
   */
  def value_=(newVal: String) = if (elem.isInstanceOf[HtmlSelect]) elem.asInstanceOf[HtmlSelect].setSelectedAttribute(newVal, true)
  else if (elem.isInstanceOf[HtmlRadioButtonInput]) {
    val radio = elem.asInstanceOf[HtmlRadioButtonInput]
    if (radio.getValueAttribute == newVal) radio.setChecked(true)
  } else if (elem.isInstanceOf[HtmlCheckBoxInput]) {
    val check = elem.asInstanceOf[HtmlCheckBoxInput]
    check.setChecked(check.getValueAttribute == newVal)
  } else if (elem.isInstanceOf[HtmlInput]) elem.asInstanceOf[HtmlInput].setValueAttribute(newVal)
  else if (elem.isInstanceOf[HtmlTextArea]) elem.asInstanceOf[HtmlTextArea].setText(newVal)
  else sys.error("unsupported setting value for element :" + elem)

  /**
   * Clicks on the element (if the click loads a new use WebPage.click instead)
   */
  def click(wait: Long = 2000) {
    page.doClick(elem, wait)
  }

}
