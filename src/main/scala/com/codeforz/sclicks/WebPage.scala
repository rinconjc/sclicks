/*
 * Copyright 2012 CodeForz
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.codeforz.sclicks

import com.gargoylesoftware.htmlunit.html._

import org.apache.commons.io.FileUtils
import java.io.{FileWriter, File}
import com.gargoylesoftware.htmlunit._
import util.WebConnectionWrapper
import annotation.tailrec
import java.net.URL
import grizzled.slf4j.Logging


trait ConnectionListener{
  def requesting(req:WebRequest)
  def responded(req:WebRequest, resp:WebResponse)
}
/**
 * Simple HTMLUnit wrapper for basic UI interactions: type and click
 */
object WebPage extends Logging{

  val CHROME_20 = new BrowserVersion("CHROME", "5.0 (Windows NT 6.2)", "Mozilla/5.0 (Windows NT 6.2) AppleWebKit/536.6 (KHTML, like Gecko) Chrome/20.0.1090.0 Safari/536.6", 20)
  val FIREFOX_11 = new BrowserVersion("Mozilla", "5.0 (Windows NT 6.1; rv:12.0)", "Mozilla/5.0 (Windows NT 6.1; rv:12.0) Gecko/20120403211507 Firefox/12.0", 12)

  def defaultClient(jsEnabled:Boolean = true, listeners:Seq[ConnectionListener])(implicit browser: BrowserVersion = BrowserVersion.FIREFOX_10) = {
    val webClient = new WebClient(browser)
    val options = webClient.getOptions
    options.setJavaScriptEnabled(jsEnabled)
    options.setThrowExceptionOnScriptError(false)
    options.setThrowExceptionOnFailingStatusCode(false)
    options.setCssEnabled(false)
    options.setRedirectEnabled(true)
    webClient.setAjaxController(new NicelyResynchronizingAjaxController)
    webClient.setJavaScriptTimeout(5000)
    webClient.getJavaScriptEngine.shutdownJavaScriptExecutor()

    if (sys.props("http.proxyHost") != null) {
      options.setProxyConfig(new ProxyConfig(sys.props("http.proxyHost"), sys.props("http.proxyPort").toInt)) //.getOptions.setProxyConfig(new ProxyConfig(sys.props("http.proxyHost"), sys.props("http.proxyPort").toInt))
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
    webClient.setRefreshHandler(new ImmediateRefreshHandler(){
      override def handleRefresh(page: Page, url: URL, seconds: Int) {
        try{
          super.handleRefresh(page, url, seconds)
        }catch{
          case e:RuntimeException if(e.getMessage.contains("Attempted to refresh a page using an ImmediateRefreshHandler")) =>
            warn("Ignoring refresh exception " + e)
          //do nothing
          case t:Throwable => throw t
        }
      }
    })
    webClient.getJavaScriptEngine.holdPosponedActions()
    webClient
  }

  /**
   * Opens the specified URL as a WebPage
   * @param url
   * @param browser the HtmlUnit browser version implementation. Defaults to Firefox-3.6 with proxy, ajax support and ignore JS errors
   * @return
   */
  def open(url: String, jsEnabled:Boolean = true, listeners:Seq[ConnectionListener]=Seq())(implicit browser: BrowserVersion = BrowserVersion.FIREFOX_10) = {
    val page: HtmlPage = defaultClient(jsEnabled, listeners)(browser).getPage(url)
    debug("Page :" + url + "==================\n" + page.asText())
    //page.setStrictErrorChecking(false)
    new WebPage(page)
  }

  /**
   * Create a new WebPage from the given URL
   * @param url
   * @return
   */
  def apply(url: String, jsEnabled:Boolean = true) = open(url, jsEnabled)
}


/**
 * A simple HtmlPage wrapper that allows usual page interactions (type, click) as well as HTML element queries
 * @param page
 */
class WebPage private(private var page: HtmlPage) extends Logging{

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

  def selectOption(selector:String, wait:Long=2000)={
    changePage(element[HtmlOption](selector).setSelected(true).asInstanceOf[HtmlPage], wait)
    this
  }

  private[sclicks] def doClick(elem: HtmlElement, wait: Long): WebPage = {
    changePage(elem.click[HtmlPage](), wait)
    this
  }

  private[sclicks] def changePage(f: =>HtmlPage, wait:Long){
    val previous = page
    //    page.getWebClient.getJavaScriptEngine.getJavaScriptExecutor.shutdown()
    page = f
    if (wait>0){
      val count = page.getWebClient.waitForBackgroundJavaScript(0)
      if (count > 0) {
        warn(count + " background scripts are still running")
        page.getWebClient.waitForBackgroundJavaScript(wait)
      }
    }
    if (previous != this.page) {
      debug("page changed to :" + page.getTitleText)
    }
  }

  def downloadText(selector:String)={
    val elem = element[HtmlElement](selector)
    elem.click[Page]() match{
      case p:TextPage =>
        val text = p.getContent
        info("Downloaded " + text.length + " chars of data!")
        Some(text)
      case x =>
        error("A TextPage was expected, but " + x  + " was returned")
        None
    }
  }

  def mouseDown(selector: String, wait: Long = 2000) = mouseAction(selector, "mousedown", wait)

  private def mouseAction(selector:String, action:String, wait:Long=2000)={
    val elem = element[HtmlElement](selector)
    val previous = page
    page = fireEvent(elem, action)
    val count = page.getWebClient.waitForBackgroundJavaScript(0)
    if (count > 0) {
      warn(count + " background scripts are still running")
      page.getWebClient.waitForBackgroundJavaScript(wait)
    }
    if (previous != this.page) {
      debug(action + " on " + elem.asXml() + ":============\n" + page.getTitleText)
    }
    this
  }

  private[sclicks] def fireEvent(elem:HtmlElement, event:String)={
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
    first(target).typeString(str)
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
   * @param valuesWaitingRefreshHandler
   */
  def fill(formSelector: String, values: (String, String)*)(wait:Long=0) {
    val form = element[HtmlForm](formSelector)
    values.foreach {
      case (k, v) =>
      val inputs = findAll[HtmlElement](form, k)
      if (inputs.isEmpty) sys.error("form element not found:" + k)
      inputs.foreach(e => if (e.isInstanceOf[HtmlSelect]) changePage(e.asInstanceOf[HtmlSelect]
        .setSelectedAttribute(v, true).asInstanceOf[HtmlPage], wait)
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
    case e => error("Failed extracting page text " + e); "--FAILED TO EXTRACT PAGE TEXT--"
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

  @tailrec
  final def waitForScripts(maxscripts:Int, maxsecs:Int=60){
    val count = page.getWebClient.waitForBackgroundJavaScript(6000)
    if (count > maxscripts){
      logger.info("scripts still running:" + count)
      if (maxsecs <= 0) warn("Timed out waiting for script completion :" + count)
      else waitForScripts(maxscripts, maxsecs - 6)
    }
  }

/*
  def waitForScripts(timeout:Long)={
    val count = page.getWebClient.waitForBackgroundJavaScript(timeout)
    logger.info("tasks running " + count)
    page.getWebClient.getJavaScriptEngine.holdPosponedActions()
    count
  }
*/

  private def element[T <: HtmlElement](selector: String): T = findElement(selector) match {
    case Some(e) => e
    case _ =>
    val file = File.createTempFile("page-", ".html")
    val fw = new FileWriter(file)
    fw.write(page.asXml())
    fw.close()
    warn("Element not found :" + selector + " in " + file.getAbsolutePath)
    sys.error("Element not found :" + selector + " in " + page.getTitleText )
  }

  private def elements[T <: HtmlElement](selector: String): Seq[T] = findAll(page.getEnclosingWindow.getTopWindow.
    getEnclosedPage.asInstanceOf[HtmlPage].getDocumentElement, selector)

  private def findElement[T <: HtmlElement](selector: String) = findFirst[T](page.getEnclosingWindow.getTopWindow.
    getEnclosedPage.asInstanceOf[HtmlPage].getDocumentElement, selector)

  def closeAll(){
    page.getWebClient.closeAllWindows()
    logger.info("All pages closed")
  }

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
   * Sets an attribute value
   * @param name
   * @param values
   */
  def attr(name: String,value:String){elem.setAttribute(name,value)}

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

  def parent = elem.getParentNode match{
    case e:HtmlElement => Some(new MatchedElement(e))
    case _ => None
  }

  def find(selector:String) = ElementFinder.findFirst[HtmlElement](elem, selector).map(new MatchedElement(_))

  def selectOption(value:String, wait:Long = 2000){
    elem match{
      case e:HtmlSelect => page.changePage(e.setSelectedAttribute(value, true).asInstanceOf[HtmlPage], wait)
      case _ =>
    }
  }

}
