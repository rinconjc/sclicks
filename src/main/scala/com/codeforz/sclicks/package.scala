package com.codeforz

import scala.util.Try

/**
 * Created by julio on 21/04/14.
 */
package object sclicks {

  def safe[T,R](pf:PartialFunction[T,R]):PartialFunction[T,Try[R]]={
    case t if pf.isDefinedAt(t) => Try(pf.apply(t))
  }

}
