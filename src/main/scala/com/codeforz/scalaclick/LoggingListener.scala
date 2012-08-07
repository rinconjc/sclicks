package com.codeforz.scalaclick

import com.gargoylesoftware.htmlunit.{WebResponse, WebRequest}
import java.util.logging.Logger

/**
 *
 */

object LoggingListener extends ConnectionListener{
  protected val logger = Logger.getLogger(this.getClass.getName)

  def requesting(req: WebRequest) {
    logger.info("Requesting:" + req.getUrl)
  }

  def responded(req: WebRequest, resp: WebResponse) {
    logger.info("Request Complete:" + req.getUrl + "\n" + resp.getContentAsString)
  }
}
