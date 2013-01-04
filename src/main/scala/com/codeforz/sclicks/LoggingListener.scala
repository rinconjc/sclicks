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

import com.gargoylesoftware.htmlunit.{WebResponse, WebRequest}
import grizzled.slf4j.Logging
import com.codeforz.sclicks.ConnectionListener

/**
 *
 */

object LoggingListener extends ConnectionListener with Logging{

  def requesting(req: WebRequest) {
    logger.info("Requesting:" + req.getUrl)
  }

  def responded(req: WebRequest, resp: WebResponse) {
    logger.info("Request Complete:" + req.getUrl + "\n" + resp.getContentAsString)
  }
}