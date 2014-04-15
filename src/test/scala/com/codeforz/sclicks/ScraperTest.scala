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
    p.waitForContent(_.find("#idres").isDefined, 20 seconds) must beASuccessfulTry[SimplePage]
    p.all("#rc h3").foreach(e=>println(e.text))
    success
  }

}
