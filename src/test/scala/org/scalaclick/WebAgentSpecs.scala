package org.scalaclick

import org.specs2.mutable.SpecificationWithJUnit


/**
 * User: rinconj
 * Date: 5/31/12 4:04 PM
 */
class WebAgentSpecs extends SpecificationWithJUnit{

  "A WebAgent" should{

    "open a URL" in {
      val agent = WebAgent("http://video.news.com.au/")
      agent.asText must_!=(null)
    }
    "click on an element" in {
      val agent = WebAgent("http://video.news.com.au/")
      val titles = agent.click(".heading :content(Most Watched)").select(".listing .video-block .heading a").map(_.text)
      println("titles count " + titles.size)
      titles foreach println
      titles must not beEmpty
    }
  }

}
