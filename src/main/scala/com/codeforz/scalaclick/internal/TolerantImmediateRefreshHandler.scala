package com.codeforz.scalaclick.internal

import com.gargoylesoftware.htmlunit.{Page, ImmediateRefreshHandler}
import java.net.URL

/**
 * 
 */
class TolerantImmediateRefreshHandler extends ImmediateRefreshHandler{
  override def handleRefresh(page: Page, url: URL, seconds: Int) {
    try{
      super.handleRefresh(page, url, seconds)
    }catch{
      case e:RuntimeException if(e.getMessage.contains("Attempted to refresh a page using an ImmediateRefreshHandler")) =>

    }
  }
}
