import models.Instruction

package object interpreter {

  type Interpreter = (Context, Instruction) => Context
}
