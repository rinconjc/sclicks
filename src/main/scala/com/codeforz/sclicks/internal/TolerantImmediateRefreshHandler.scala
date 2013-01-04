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

package com.codeforz.sclicks.internal

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
