package org.scalaclick

import util.parsing.combinator.RegexParsers
import util.matching.Regex
import org.apache.log4j.Logger

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

object SelectorParser extends RegexParsers {
  private val logger = Logger.getLogger(this.getClass)

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
   */
  lazy val byAttr = regexMatch( """([^\[]+)\[([^*=]+)([^']+)'([^']+)'\]""".r) ^^ {
    case m => ByAttr(m.group(1), m.group(2), m.group(3), m.group(4))
  }
  /**
   * Matches an element by tag name. e.g. elem
   */
  lazy val byTag = regexMatch("([^\\s:]+)".r) ^^ {
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
    logger.debug("Parsed selectors:" + result)
    result.getOrElse(Nil)
  }
}

