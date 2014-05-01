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
import java.io._
import com.gargoylesoftware.htmlunit._
import util.WebConnectionWrapper
import java.net.URL
import scala.concurrent.{Await, Promise}
import scala.util.{Failure, Try}
import concurrent.duration._
import com.gargoylesoftware.htmlunit.attachment.AttachmentHandler
import com.codeforz.sclicks.ElementFinder._
import scala.Some
import java.util.concurrent.TimeoutException
import com.typesafe.scalalogging.slf4j.LazyLogging

trait ConnectionListener {
  def requesting(req: WebRequest)

  def responded(req: WebRequest, resp: WebResponse)
}

trait PageEvent
case class NodeAdded(node:DomNode) extends PageEvent
case class NodeRemoved(elem:DomNode) extends PageEvent


/**
 * Simple HTMLUnit wrapper for basic UI interactions: type and click
 */
object SimpleWebClient extends LazyLogging {

  val CHROME_20 = new BrowserVersion("CHROME", "5.0 (Windows NT 6.2)", "Mozilla/5.0 (Windows NT 6.2) AppleWebKit/536.6 (KHTML, like Gecko) Chrome/20.0.1090.0 Safari/536.6", 20)
  val FIREFOX_11 = new BrowserVersion("Mozilla", "5.0 (Windows NT 6.1; rv:12.0)", "Mozilla/5.0 (Windows NT 6.1; rv:12.0) Gecko/20120403211507 Firefox/12.0", 12)

  def defaultClient(jsEnabled: Boolean = true, listeners: Seq[ConnectionListener])(implicit browser: BrowserVersion = BrowserVersion.FIREFOX_17) = {
    val webClient = new WebClient(browser)
    val options = webClient.getOptions
    options.setJavaScriptEnabled(jsEnabled)
    options.setThrowExceptionOnScriptError(false)
    options.setThrowExceptionOnFailingStatusCode(false)
    options.setCssEnabled(false)
    options.setRedirectEnabled(true)
    //    webClient.setAjaxController(new NicelyResynchronizingAjaxController)
    //    webClient.setJavaScriptTimeout(5000)
    //    webClient.getJavaScriptEngine.shutdownJavaScriptExecutor()

    sys.props.get("http.proxyHost").foreach {
      proxy =>
        options.setProxyConfig(new ProxyConfig(proxy, sys.props("http.proxyPort").toInt)) //.getOptions.setProxyConfig(new ProxyConfig(sys.props("http.proxyHost"), sys.props("http.proxyPort").toInt))
    }
    sys.props.get("sclicksProxyHost").foreach {
      proxy =>
        options.setProxyConfig(new ProxyConfig(proxy, sys.props("sclicksProxyPort").toInt, sys.props.get("sclicksProxySocks").exists(_.toBoolean)))
    }

    if (listeners.nonEmpty) {
      //wrap connection
      webClient.setWebConnection(new WebConnectionWrapper(webClient.getWebConnection) {
        override def getResponse(request: WebRequest) = {
          listeners.foreach(_.requesting(request))
          val response = super.getResponse(request)
          listeners.foreach(_.responded(request, response))
          response
        }
      })
    }
    webClient.setRefreshHandler(new ImmediateRefreshHandler() {
      override def handleRefresh(page: Page, url: URL, seconds: Int) {
        try {
          super.handleRefresh(page, url, seconds)
        } catch {
          case e: RuntimeException if e.getMessage.contains("Attempted to refresh a page using an ImmediateRefreshHandler") =>
            logger.warn("Ignoring refresh exception " + e)
          //do nothing
          case t: Throwable => throw t
        }
      }
    })
    //  webClient.getJavaScriptEngine.holdPosponedActions()
    webClient
  }

  /**
   * Opens the specified URL as a WebPage
   * @param url
   * @param browser the HtmlUnit browser version implementation. Defaults to Firefox-3.6 with proxy, ajax support and ignore JS errors
   * @return
   */
  def open(url: String, jsEnabled: Boolean = true, listeners: Seq[ConnectionListener] = Seq())(implicit browser: BrowserVersion = BrowserVersion.FIREFOX_17) = {
    val client: WebClient = defaultClient(jsEnabled, listeners)(browser)
    client.getPage(url)
    new SimpleWebClient(client)
  }

  /**
   * Create a new WebPage from the given URL
   * @param url
   * @return
   */
  def apply(url: String, jsEnabled: Boolean = true) = open(url, jsEnabled)

}


/**
 * A simple HtmlPage wrapper that allows usual page interactions (type, click) as well as HTML celement queries
 * @param page
 */
class SimpleWebClient private(private val client: WebClient) extends LazyLogging {


  private implicit val thisPage: SimpleWebClient = this

  client.addWebWindowListener(new WebWindowListener {
    override def webWindowOpened(event: WebWindowEvent): Unit = {
      logger.debug(s"window opened ${event.getWebWindow.getName}")
    }

    override def webWindowClosed(event: WebWindowEvent): Unit = {
      logger.debug(s"window close ${event.getWebWindow.getName}")
    }

    override def webWindowContentChanged(event: WebWindowEvent): Unit = {
      logger.debug(s"window content changed ${event.getWebWindow.getName}")
    }
  })

  def currentWindow = new SimpleWebWindow(client.getCurrentWindow)

  def withCurrentPage[T](actions: SimpleWebWindow=>T):T={
    actions(currentWindow)
  }

  def withPageInWindow[T](window:String, actions:SimpleWebWindow =>T):T={
    actions(new SimpleWebWindow(client.getWebWindowByName(window)))
  }

  def waitForContent(condition:SimpleWebWindow=>Boolean, maxWait:Duration = 10 seconds)={
    val p = Promise[Unit]()
    val listener = new SimpleWindowListener(changed = {event=>
      val newPage = new SimpleWebWindow(event.getWebWindow)
      if(condition(newPage)) {
        p.success()
      }
      else logger.warn("page changed but didn't satisfy condition.")
    })
    client.addWebWindowListener(listener)
    Await.result(p.future, maxWait)
    client.removeWebWindowListener(listener)
  }

  def closeAll() {
    client.closeAllWindows()
    logger.info("All pages closed")
  }

}

class SimpleWebWindow(private val window:WebWindow) extends LazyLogging{
  private implicit val self  = this

  def page = window.getEnclosedPage.asInstanceOf[HtmlPage]
  /**
   * Simulates a click on the element matching the specified selector. The click action may result in a new page loading
   * in which case page field will be update to the new page.
   *
   * @param selector An element select (see Selectors)
   * @return this same instance
   */
  def click(selector: String) = {
    val elem = element[HtmlElement](selector)
    pageAfterAction(elem.click[HtmlPage]())
  }

  def selectOption(selector: String) = {
    pageAfterAction(element[HtmlOption](selector).setSelected(true).asInstanceOf[HtmlPage])
  }

  private[sclicks] def pageAfterAction(f: =>HtmlPage)={
    val curPage = page
    val newPage = f
    if(newPage != curPage){
      logger.debug(s"page changed to ${newPage.getTitleText}")
    }
    this
  }

  /**
   * Page title
   * @return
   */
  def title = page.getTitleText

  def downloadText(selector: String) = {
    val elem = element[HtmlElement](selector)
    elem.click[Page]() match {
      case p: TextPage =>
        val text = p.getContent
        logger.info("Downloaded " + text.length + " chars of data!")
        Some(text)
      case x =>
        logger.error("A TextPage was expected, but " + x + " was returned")
        None
    }
  }

  /**
   * Tries to download an attachement by clicking on the specified selector
   */
  def getAttachment(selector:String, timeoutSecs:Int=30)={
    val prevHandler = page.getWebClient.getAttachmentHandler
    val promise = Promise[Page]()
    page.getWebClient.setAttachmentHandler(new AttachmentHandler {
      def handleAttachment(page: Page){
        logger.debug(s"attachment received $page")
        promise.success(page)
      }
    })

    element[HtmlElement](selector).click[Page]()

    val streamOpt = Try(Await.result(promise.future, timeoutSecs seconds)).map(pageToStream).getOrElse {
      logger.warn("timed out waiting for attachment")
      None
    }
    page.getWebClient.setAttachmentHandler(prevHandler)
    streamOpt
  }

  private def pageToStream(resultPage: Page): Option[InputStream] = {
    resultPage match {
      case p: TextPage =>
        val text = p.getContent
        logger.info("Downloaded " + text.length + " chars of data!")
        Some(new ByteArrayInputStream(text.getBytes("UTF-8")))
      case b: BinaryPage =>
        logger.info("Binary page found")
        Some(b.getInputStream)
      case u: UnexpectedPage =>
        logger.info("unexpected page found")
        Some(u.getInputStream)
      case x =>
        logger.error("A TextPage was expected, but " + x + " was returned")
        logger.debug("dumping page content:" + x.getWebResponse.getContentAsString)
        None
    }
  }

  def mouseDown(selector: String) = mouseAction(selector, "mousedown")

  private def mouseAction(selector: String, action: String) = {
    val elem = element[HtmlElement](selector)
    pageAfterAction(fireEvent(elem, action))
  }

  private[sclicks] def fireEvent(elem: HtmlElement, event: String) = {
    val result = elem.fireEvent(event)
    if (result == null) page
    else result.getNewPage.asInstanceOf[HtmlPage]
  }

  /**
   * Returns all elements matching the given selector
   * @param selector
   * @return
   */
  def all(selector: String) = elements[HtmlElement](selector).map(MatchedElement(_))

  /**
   * Returns first element matching the given selector
   * @param selector
   * @return
   */
  def first(selector: String) = MatchedElement(element[HtmlElement](selector))

  /**
   * Finds the element matching the given selector
   * @param selector
   * @return
   */
  def find(selector: String) = findElement[HtmlElement](selector).map(MatchedElement(_))

  /**
   * Types the string into the element matching the target selector
   * @param target
   * @param str
   */
  def typeIn(target: String, str: String) {
    first(target).typeIn(str)
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
  def fill(formSelector: String, values: (String, String)*)(wait: Int = 0) {
    val form = element[HtmlForm](formSelector)
    values.foreach {
      case (k, v) =>
        val inputs = findAll[HtmlElement](form, k)
        if (inputs.isEmpty) sys.error("form element not found:" + k)
        inputs.foreach {
          case select: HtmlSelect => select.setSelectedAttribute(v, true)
          case radio: HtmlRadioButtonInput =>
            if (radio.getValueAttribute == v) radio.setChecked(true)
          case check: HtmlCheckBoxInput =>
            check.setChecked(check.getValueAttribute == v)
          case input: HtmlInput => input.setValueAttribute(v)
          case area: HtmlTextArea => area.setText(v)
          case _ => sys.error("unsupported element for form fill:" + k)
        }
    }
  }

  /**
   * Returns the page text content
   * @return Page text content
   */
  def asText = try {
    page.asText()
  } catch {
    case e:Exception => error("Failed extracting page text " + e)
      "--FAILED TO EXTRACT PAGE TEXT--"
  }

  def saveTo(file: String) {
    FileUtils.writeStringToFile(new File(file), page.asXml())
  }

  /**
   * Extracts values of the elements matched by the given selector. Only HTML input and option elements are supported
   * @param selector
   * @return
   */
  def values(selector: String): Iterator[String] = elements[HtmlElement](selector).map {
    case option: HtmlOption => option.getValueAttribute
    case input: HtmlInput => input.getValueAttribute
    case area: HtmlTextArea => area.getText
    case e => sys.error("Element of type " + e.getClass + " does not support values")
  }

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
      sys.error("No Ok response received for " + path + "."  + response.getStatusCode + ":" + response.getStatusMessage)
    }
    else response.getContentAsStream
  }

  private def element[T <: HtmlElement](selector: String): T = findElement[T](selector) match {
    case Some(e) => e
    case _ =>
      val file = File.createTempFile("page-", ".html")
      val fw = new FileWriter(file)
      fw.write(page.asXml())
      fw.close()
      logger.warn("Element not found :" + selector + " in " + file.getAbsolutePath)
      sys.error("Element not found :" + selector + " in " + page.getTitleText)
  }

  private def elements[T <: HtmlElement](selector: String) = findAll[T](page.getDocumentElement, selector)

  private def findElement[T <: HtmlElement](selector: String) = findFirst[T](topElement, selector)

  private def topElement[T <: HtmlElement]: HtmlElement = {
    page match {
      case html:HtmlPage => html.getDocumentElement
      case other =>
        logger.warn(s"non html page found in root $other falling back to current page $page")
        page.getDocumentElement
    }
  }

  def whenContentChanges[T](rule:PartialFunction[SimpleWebWindow,T], maxWait:Duration=10 seconds)={
    val p = Promise[T]()
    val self = this
    val listener = new DomChangeListener {
      val handler = safe(rule).runWith{res=>
        p.complete(res)
        page.removeDomChangeListener(this)
      }
      override def nodeAdded(event: DomChangeEvent): Unit = {
        logger.debug(s"node added $event")
        handler(self)
      }
      override def nodeDeleted(event: DomChangeEvent): Unit = {
        logger.debug(s"node removed $event")
        handler(self)
      }
    }
    page.addDomChangeListener(listener)
    Try(Await.result(p.future, maxWait)).recoverWith{
      case e:Exception =>
        page.removeDomChangeListener(listener)
        Failure(e)
    }.recoverWith{
      case e:TimeoutException if(rule.isDefinedAt(self)) =>
        logger.warn("page change detection missed")
        Try(rule(self))
    }
  }

  /**
  *
  */
  def waitForContent(check:SimpleWebWindow=>Boolean, maxWait:Duration=10 seconds)={
    whenContentChanges{case _ if check(this) => this}
  }

  def refresh()={
    page.refresh()
    this
  }
}

/**
 * A simple wrapper around HTML Elements to provide common and most used methods
 * @param elem
 */
case class MatchedElement(elem: HtmlElement)(implicit page: SimpleWebWindow) extends LazyLogging{
  import collection.JavaConversions._
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
   * Determines if element is selected applicable to html option and checkbox only
   */
  def selected = elem match{
    case option:HtmlOption => option.isSelected
    case checkbox:HtmlCheckBoxInput => checkbox.isChecked
    case other => false
  }

  /**
   * Sets an attribute value
   * @param name
   * @param value
   */
  def attr(name: String, value: String) {
    elem.setAttribute(name, value)
  }

  /**
   * Value of the element. Only Form elements are supported i.e html input, option, select(single value), and textarea
   * @return
   */
  def value = elem match {
    case input: HtmlInput => input.getValueAttribute
    case option: HtmlOption => option.getValueAttribute
    case area: HtmlTextArea => area.getText
    case select: HtmlSelect => select.getSelectedOptions.get(0).getValueAttribute
    case _ => sys.error("Value not supported in element " + elem)
  }

  /**
   * Types the string into the element
   * @param str
   */
  def typeIn(str: String) {
    val selectable = elem.asInstanceOf[ {def select()}]
    selectable.select()
    elem.`type`(str)
  }

  /**
   * Sets the value of the elements
   * @param newVal
   * @return
   */
  def value_=(newVal: String) = elem match {
    case select: HtmlSelect => select.setSelectedAttribute(newVal, true)
    case radio: HtmlRadioButtonInput =>
      if (radio.getValueAttribute == newVal) radio.setChecked(true)
    case check: HtmlCheckBoxInput =>
      check.setChecked(check.getValueAttribute == newVal)
    case input: HtmlInput => input.setValueAttribute(newVal)
    case area: HtmlTextArea => area.setText(newVal)
    case _ => sys.error("unsupported setting value for element :" + elem)
  }

  /**
   * Clicks on the element (if the click loads a new use WebPage.click instead)
   */
  def click()={
    page.pageAfterAction(elem.click())
  }

  def parent = elem.getParentNode match {
    case e: HtmlElement => Some(new MatchedElement(e))
    case _ => None
  }

  def find(selector: String) = ElementFinder.findFirst[HtmlElement](elem, selector).map(new MatchedElement(_))

  def all(selector:String) = ElementFinder.findAll[HtmlElement](elem, selector).map(new MatchedElement(_))

  def selectOption(value: String) = {
    elem match {
      case e: HtmlSelect if ! e.getSelectedOptions.exists(_.getValueAttribute == value) =>
        page.pageAfterAction(e.setSelectedAttribute(value, true).asInstanceOf[HtmlPage])
      case _ =>
    }
  }

  def visible = elem.isDisplayed

  def hidden = !elem.isDisplayed

  def whenAttrChanges(name:String)(f:Option[String]=>Boolean)={
    val p = Promise[Unit]()
    val listener = new HtmlAttributeChangeListener {
      def checkAttribute(attr: String, valueOpt: Option[String]) {
        logger.debug(s"attribute $attr changed to $valueOpt")
        if (name.equalsIgnoreCase(attr) && f(valueOpt)) {
          elem.removeHtmlAttributeChangeListener(this)
          p.trySuccess(())
          logger.debug(s"promise completed $p")
        }
      }

      def attributeAdded(event: HtmlAttributeChangeEvent) = {
        logger.debug(s"added: ${event.getName}=${event.getValue}")
        checkAttribute(event.getName, Some(event.getValue))
      }

      def attributeReplaced(event: HtmlAttributeChangeEvent) = {
        logger.debug(s"replaced:${event.getName}=${event.getValue} to ${elem.getAttribute(name)}")
        checkAttribute(event.getName, Some(elem.getAttribute(name)))
      }

      def attributeRemoved(event: HtmlAttributeChangeEvent) = {
        logger.debug(s"removed: ${event.getName}")
        checkAttribute(event.getName, None)
      }
    }
    elem.addHtmlAttributeChangeListener(listener)
    p.future
  }

  def whenDomChanges(check:MatchedElement=>Boolean)={
    val p = Promise[Unit]()
    val self = this
    elem.addDomChangeListener(new DomChangeListener {
      def evalChange(event:DomChangeEvent){
        logger.debug(s"dom changed: ${event.getChangedNode}")
        if(check(self)){
          logger.debug("dom change completed")
          p.success(())
          elem.removeDomChangeListener(this)
        }
      }
      def nodeDeleted(event: DomChangeEvent) = {
        evalChange(event)
      }
      def nodeAdded(event: DomChangeEvent) = {
        evalChange(event)
      }
    })
    p.future
  }
}

private class SimpleWindowListener(opened:WebWindowEvent=>Unit=_=>(), changed:WebWindowEvent=>Unit=_=>()
                                 , closed:WebWindowEvent=>Unit=_=>()) extends WebWindowListener with LazyLogging{
  def webWindowClosed(event: WebWindowEvent){
    closed(event)
  }
  def webWindowContentChanged(event: WebWindowEvent){
    changed(event)
  }

  def webWindowOpened(event: WebWindowEvent){
    opened(event)
  }
}

