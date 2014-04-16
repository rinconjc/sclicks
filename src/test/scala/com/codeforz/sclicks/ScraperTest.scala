package com.codeforz.sclicks

import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.time.NoTimeConversions

/**
 * Created by julio on 13/04/14.
 */
class ScraperTest extends SpecificationWithJUnit with NoTimeConversions{
  import concurrent.duration._
  import SimpleWebClient._
  "open a simple page" in{
    val client = open("http://google.com")
    val p = client.currentPage
    p.title must_== "Google"
    p.first("*[name='q']").typeIn("hearthbleed")

    val newPage = p.click("*[name='btnG']").waitForContent(_.find("#ires").isDefined, 20 seconds)
    newPage must beASuccessfulTry[SimplePage]

    newPage.foreach{p=>
      p.all(".rc h3").foreach(e=> println(s"${e.text} -> ${e.find("a").map(_.attr("href"))}"))
    }
    client.closeAll()
    success
  }

}
