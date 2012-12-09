package com.codeforz.scalaclick

import com.gargoylesoftware.htmlunit.{WebResponse, WebRequest}
import grizzled.slf4j.Logging

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
