package com.codeforz.sclicks

import org.specs2.mutable.SpecificationWithJUnit
import scala.util.Success
import org.specs2.time.NoTimeConversions

/**
 * Created by julio on 13/04/14.
 */
class ScraperTest extends SpecificationWithJUnit with NoTimeConversions{
  import concurrent.duration._
  import concurrent.ExecutionContext.Implicits.global
  import WebPage._
  "open a simple page" in{
    val p = open("http://google.com", listeners = Seq(LoggingListener))
    p.title must_== "Google"
    p.first("*[name='q']").typeIn("hearthbleed")
    p.first("body").whenDomChanges(_.find("#idres").isDefined).andThen{
      case Success(_) => p.all("#rc h3") foreach {e=> println(e.text)}
    } must be_==(()).await(timeout = 25 seconds)
  }

}
