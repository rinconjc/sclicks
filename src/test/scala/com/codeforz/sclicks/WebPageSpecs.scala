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

import org.specs2.mutable.SpecificationWithJUnit


/**
 * User: rinconj
 * Date: 5/31/12 4:04 PM
 */
class WebPageSpecs extends SpecificationWithJUnit {

  "A WebPage" should {

    "open a URL" in {
      val agent = SimpleWebClient("http://video.news.com.au/")
      agent.currentWindow.asText must_!= (null)
    }
    /*"click on an element" in {
      val agent = WebPage("http://video.news.com.au/")
      agent.waitForScripts(0)
      val titles = agent.click(".heading :content(Most Watched)").all(".listing .video-block .heading a").map(_.text)
      println("titles count " + titles.size)
      titles foreach println
      titles must not beEmpty      
    }*/
  }

}
