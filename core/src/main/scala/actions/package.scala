package com.github.philcali.puppet

import lmxml.LmxmlNode
import dispatch.Promise

package object actions {
  type ActionReturn = ((LmxmlNode, Context), Option[String])
}
