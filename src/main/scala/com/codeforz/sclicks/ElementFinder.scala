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

import com.gargoylesoftware.htmlunit.html.{HtmlPage, BaseFrame, HtmlElement}
import collection.JavaConversions._
import grizzled.slf4j.Logging

/**
 * Base element finder
 */
trait ElementFinder extends Logging{

  /**
   * Finds elements matching this finder
   * @param node
   * @return
   */
  def findElements(node: HtmlElement): Seq[HtmlElement] = {
    debug("Finding elements in node %s with finder %s\n".format(node, this))
    if (node.isInstanceOf[BaseFrame]) {
      val newnode = node.asInstanceOf[BaseFrame].getEnclosedPage.asInstanceOf[HtmlPage].getDocumentElement
      debug("search node was a frame, replacing with frame inner document %s\n".format(newnode))
      findElements(newnode)
    } else find(node)
  }

  /**
   * Finds all matching elements
   * @param node
   * @return
   */
  protected def find(node: HtmlElement): Seq[HtmlElement]
}

/**
 * A simple html element finder based on Jquery like selectors
 */
object ElementFinder {

  /**
   * Finds all elements descendents of the root element and matching the given selector
   * @param root
   * @param selector
   * @tparam T
   * @return
   */
  def findAll[T <: HtmlElement](root: HtmlElement, selector: String): Seq[T] = {
    def elements(seq: Seq[ElementFinder], nodes: Seq[HtmlElement]): Seq[HtmlElement] = {
      if (seq.isEmpty || nodes.isEmpty) nodes
      else nodes.flatMap {
        node => elements(seq.tail, seq.head.findElements(node))
      }
    }

    elements(SelectorParser.parse(selector), Seq(root)).asInstanceOf[Seq[T]]
  }

  /**
   * Finds the first descendant element of root that matches the given selector
   * @param root
   * @param selector
   * @tparam T
   * @return
   */
  def findFirst[T <: HtmlElement](root: HtmlElement, selector: String): Option[T] = {
    val elements: Seq[T] = findAll[T](root, selector)
    if (elements.size > 0) Some(elements(0)) else None //sys.error("No element found")
  }
}


/**
 * Simple finder that finds elements by id
 * @param id
 */
case class ById(id: String) extends ElementFinder {
  def find(node: HtmlElement): Seq[HtmlElement] = try {
    List(node.getElementById[HtmlElement](id))
  } catch {
    case e => warn("Element "+ id + "not found in " + node.getNodeName + "\n" + e ); Nil
  }
}

/**
 * Simple finder that finds elements by css class name
 * @param clss
 */
case class ByClass(clss: String) extends ElementFinder {
  def find(node: HtmlElement): Seq[HtmlElement] = node.getHtmlElementDescendants.filter(
    _.getAttribute("class").split("\\s+").contains(clss)).toSeq // node.querySelectorAll(clss).map(_.asInstanceOf[HtmlElement])
}

/**
 * Simple finder that finds elements by tag name
 * @param tag
 */
case class ByTag(tag: String) extends ElementFinder {
  def find(node: HtmlElement): Seq[HtmlElement] = node.getElementsByTagName(tag)
}

/**
 * Finds elements by attribute expression.
 * @param tag element tag name. * to indicate any element tag
 * @param attr attribute name
 * @param op = exact match, *= partial match
 * @param value value predicate
 */
case class ByAttr(tag: String, attr: String, op: String, value: String) extends ElementFinder {
  val filter = (e:HtmlElement) => if (op == "=") e.getAttribute(attr) == value
  else if (op == "*=") e.getAttribute(attr).contains(value)
  else sys.error("Unsupported operator " + op)

  def find(node: HtmlElement) = {
    if (tag=="*") node.getHtmlElementDescendants.filter(filter).toSeq
    else node.getElementsByTagName(tag).filter(filter)
  }
}

/**
 * Finds elements by text content
 * @param text
 */
case class ByContent(text: String) extends ElementFinder {
  protected def find(node: HtmlElement) = if (node.getTextContent.contains(text)) Seq(node) else Seq() //getChildElements.filter(_.getTextContent.contains(text)).toSeq
}
