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

import util.parsing.combinator.RegexParsers
import util.matching.Regex
import scala.Some
import grizzled.slf4j.Logging

/**
 * A JQuery like html element selector parser.
 * Supported selectors:
 * id => #elementId
 * css class => .some_class
 * attribute => tag[attribute*=value]
 * content => :content(some text)
 * ...
 * selectors can be chained together to provide successive filtering. e.g div .item a[title*='link']
 *
 * Date: 11/30/11 10:52 AM
 */

object SelectorParser extends RegexParsers with Logging {

  def regexMatch(r: Regex): Parser[Regex.Match] = new Parser[Regex.Match] {
    def apply(in: Input) = {
      val source = in.source
      val offset = in.offset
      val start = handleWhiteSpace(source, offset)
      (r findPrefixMatchOf (source.subSequence(start, source.length))) match {
        case Some(matched) =>
          Success(matched,
            in.drop(start + matched.end - offset))
        case None =>
          Failure("string matching regex `" + r + "' expected but `" + in.first + "' found", in.drop(start - offset))
      }
    }
  }

  lazy val selector = term *
  lazy val term = byId | byClass | byAttr | byContent | byTag

  /**
   * Matches an element id selectors. e.g. #someid
   */
  lazy val byId = regexMatch("#([^\\s]+)".r) ^^ {
    case m => ById(m.group(1))
  }

  /**
   * Matches an element selector by class .some_css_class
   */
  lazy val byClass = regexMatch("\\.([^\\s]+)".r) ^^ {
    case m => ByClass(m.group(1))
  }

  /**
   * Matches elements selector by attribute e.g.
   * element[attr_name='attr_value'] //exact match
   * element[attr_name*='attr_value'] // partial match: attribute contains attr_value
   * *[attr_name='attr_val'] // any element with the given attributes
   */
  lazy val byAttr = regexMatch( """([^\s\[]+)\[([^*=]+)([^']+)'([^']+)'\]""".r) ^^ {
    case m => ByAttr(m.group(1), m.group(2), m.group(3), m.group(4))
  }
  /**
   * Matches an element by tag name. e.g. elem
   */
  lazy val byTag = regexMatch("(\\p{Alpha}+)".r) ^^ {
    case m => ByTag(m.group(1))
  }
  /**
   * Matches an element by text content. e.g :content(some text)
   */
  lazy val byContent = regexMatch( """:content\(([^\)]+)\)""".r) ^^ {
    case m => ByContent(m.group(1))
  }

  /**
   * Parses the input as a element selector chain
   * @param input
   * @return The selector matchers
   */
  def parse(input: String) = {
    val result = this.parseAll(selector, input)
    debug("Parsed selectors:" + result)
    result.getOrElse(Nil)
  }
}

