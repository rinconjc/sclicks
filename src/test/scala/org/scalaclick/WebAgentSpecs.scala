package org.scalaclick

import org.specs2.mutable.SpecificationWithJUnit


/**
 * User: rinconj
 * Date: 5/31/12 4:04 PM
 */
object WebAgentSpecs extends SpecificationWithJUnit{
  "A WebAgent" should{
    "open a URL" in {
      val agent = WebAgent("http://video.news.com.au/")
      agent.asText must_!=(null)
    }
  }

}
