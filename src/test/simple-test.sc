/**
 * User: rinconj
 * Date: 4/16/13 12:03 PM
 */
import com.codeforz.sclicks.browser.Browser
val b = new Browser

val wp = b.get("http://google.com")

println(s"title: ${wp.title}\nxml:\n${wp.asXml}")







































































































