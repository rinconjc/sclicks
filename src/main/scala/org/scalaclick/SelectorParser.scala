package org.scalaclick

import util.parsing.combinator.RegexParsers
import util.matching.Regex
import org.apache.log4j.Logger

/**
 * User: rinconj
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
  lazy val byId = regexMatch("#([^\\s]+)".r) ^^ {
    case m => ById(m.group(1))
  }
  lazy val byClass = regexMatch("\\.([^\\s]+)".r) ^^ {
    case m => ByClass(m.group(1))
  }
  lazy val byAttr = regexMatch( """([^\[]+)\[([^*=]+)([^']+)'([^']+)'\]""".r) ^^ {
    case m => ByAttr(m.group(1), m.group(2), m.group(3), m.group(4))
  }
  lazy val byTag = regexMatch("([^\\s:]+)".r) ^^ {
    case m => ByTag(m.group(1))
  }
  lazy val byContent = regexMatch( """:content\(([^\)]+)\)""".r) ^^ {
    case m => ByContent(m.group(1))
  }


  def parse(input: String) = {
    val result = this.parseAll(selector, input)
    logger.debug("Parsed selectors:" + result)
    result.getOrElse(Nil)
  }
}

