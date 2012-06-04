package org.scalaclick

import org.specs2.mutable.SpecificationWithJUnit


/**
 * User: rinconj
 * Date: 11/30/11 11:11 AM
 */

class SelectorParserSpecs extends SpecificationWithJUnit {
  "selector parser" should {
    "parse a simple id" in {
      SelectorParser.parse("#someid") mustEqual List(ById("someid"))
    }
  }

}