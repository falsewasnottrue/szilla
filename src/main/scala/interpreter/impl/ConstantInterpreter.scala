package interpreter.impl
import interpreter.{Context, Global}
import models._

object ConstantInterpreter extends BaseInterpreter {
  // defines a constant
  override def apply(ctx: Context)(i: Instruction): Context = {
    val Seq(StringValue(name), value) = arguments(ctx)(i, ValueTypes(StringType, WildcardType))
    Global.set(Variable(name), value)
    ctx
  }
}
