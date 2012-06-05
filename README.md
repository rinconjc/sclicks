scala-click
===========

A Simple HTMLUnit wrapper for basic headless browsing interactions (type and click) as well as data extraction. It can be used for scraping websites that implement HTTP sessions and Ajax functionality, e.g. extracting bank account transactions.

        import org.scalaclick.WebPage._
        import java.net.URLDecoder._

        val page = open("http://video.news.com.au/")
        //get the most watched videos
        val titles = page.click(".heading :content(Most Watched)").all(".listing .video-block .heading a").map(_.text)

        titles foreach println

        //now let's get the urls from google

        val google = open("https://www.google.com.au")
        var urls = titles.map{title=>
            google.first("input[name='q']").value = "\"" + title + "\""
            google.click("button[name='btnG']")
            //
            google.all("li .g a").find(_.text.contains("Video: " + title )).map(e=>"url=([^&$]+)".r.findFirstMatchIn(e.attr ("url")).map(_.group(1))).flatten
        }.flatten.map(decode(_,"UTF-8"))

        urls foreach println


* Implements JQuery like selectors to find elements in a page. e.g.

*



