//object Macros {
//  def foo(x: Int) = macro Impls.foo
//}

import scala.tools.nsc.reporters._
import scala.tools.nsc.Settings
import reflect.runtime.Mirror.ToolBox

object Test extends App {
  import scala.reflect.mirror._
  
  val macrobody = Select(Ident(newTermName("Impls")), newTermName("foo"))
  val macroparam = ValDef(NoMods, newTermName("x"), TypeTree(definitions.IntClass.asType), EmptyTree)
  val macrodef = DefDef(Modifiers(Set(scala.reflect.api.Modifier.`macro`)), newTermName("foo"), Nil, List(List(macroparam)), TypeTree(), macrobody)
  val modulector = DefDef(NoMods, nme.CONSTRUCTOR, Nil, List(List()), TypeTree(), Block(Apply(Select(Super(This(EmptyTypeName), EmptyTypeName), nme.CONSTRUCTOR), List())))
  val module = ModuleDef(NoMods, newTermName("Macros"), Template(List(Select(Ident(newTermName("scala")), newTypeName("ScalaObject"))), emptyValDef, List(modulector, macrodef)))
  val macroapp = Apply(Select(Ident("Macros"), newTermName("foo")), List(Literal(Constant(42)))) 
  val tree = Block(macrodef, module, macroapp)

  val stderr = new java.io.ByteArrayOutputStream()
  Console.setErr(new java.io.PrintStream(stderr))

  val reporter = new ConsoleReporter(new Settings)
  val toolbox = new ToolBox(reporter)
  try { println(toolbox.runExpr(tree)) }
  catch { case ex: Throwable => println(stderr); println(ex) }
}
