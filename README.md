scala-click
===========

A Simple HTMLUnit wrapper for headless browsing (type and click) as well as data extraction. It can be used for scraping websites that implement HTTP sessions or Ajax functionality, e.g. extracting bank account transactions.

Here's a simple example, where we get the most watched videos from a news video site, then we search google to retrieve their URLs

        import org.scalaclick.WebPage._
        import java.net.URLDecoder._

        val page = open("http://video.news.com.au/")
        //get the most watched videos
        val titles = page.click(".heading :content(Most Watched)").all(".listing .video-block .heading a").map(_.text)

        titles foreach println

        //now let's get the urls from google
        val google = open("https://www.google.com.au")(FIREFOX_11)
        var urls = titles.map{title=>
            google.first("input[name='q']").typeString("\"" + title + "\"")
            google.click("form *[name='btnG']") //google flips the search button as button and input...
            google.all("#ires .g a").find(_.text.contains("Video: " + title )).map(e=>"q=([^&$]+)".r.findFirstMatchIn(e.attr("href")).map(_.group(1))).flatten
        }.flatten.map(decode(_,"UTF-8"))

        urls foreach println

* Supports the following JQuery-like selectors:
    * Id selector e.g. `#some_id`
    * CSS Class selector e.g. `.some_class`
    * Attribute selector e.g.  `input[name='username']` or `a[href*='jpg']` or `*[name='btn']`
    * Text content selector e.g. :content(some text)
    * Element tag selector e.g. `table`
    * Chained selectors e.g. `#container .data table td `



