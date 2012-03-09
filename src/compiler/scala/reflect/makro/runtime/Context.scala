package scala.reflect.makro
package runtime

import scala.tools.nsc.Global

abstract class Context(val mirror: Global) extends scala.reflect.makro.Context
                                           with Aliases
                                           with Reifiers
                                           with Reporters
                                           with Symbols
                                           with Typers