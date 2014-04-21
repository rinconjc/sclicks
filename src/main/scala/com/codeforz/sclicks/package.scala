package com.codeforz

import scala.util.Try

/**
 * Created by julio on 21/04/14.
 */
package object sclicks {

  def safe[T,R](pf:PartialFunction[T,R]):PartialFunction[T,Try[R]]=new PartialFunction[T, Try[R]]{
    def isDefinedAt(t:T) = pf.isDefinedAt(t)
    def apply(t:T) = Try(pf.apply(t))
  }

}
