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
    "parse with filter" in{
      SelectorParser.parse("p:content(text)") mustEqual List(Filtered(ByTag("p"), ByContent("text")))
      SelectorParser.parse("h2:last") mustEqual List(Filtered(ByTag("h2"), ByIndex(-1)))
      SelectorParser.parse(".class1:first") mustEqual List(Filtered(ByClass("class1"), ByIndex(1)))
      SelectorParser.parse("input[type='checkbox']:eq(2)") mustEqual List(Filtered(ByAttr("input","type","=","checkbox"), ByIndex(2)))
    }
    "parse with children" in{
      SelectorParser.parse("div span:last") mustEqual List(ByTag("div"), Filtered(ByTag("span"), ByIndex(-1)))
    }
  }

}