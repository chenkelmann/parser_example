package eu.henkelmann.parser02


sealed trait Expression {
    def eval(env:Map[String,Literal]): Literal
}

sealed trait Literal extends Expression {
    def eval(env:Map[String, Literal]) = this
    def doubleValue:Double
    def boolValue:Boolean
    def stringValue:String
}

case class NumberLiteral(literal:Double) extends Literal{
    def doubleValue = literal
    def boolValue   = literal != 0.0
    def stringValue = literal.toString
    override def toString  = literal.toString
}

case class BooleanLiteral(literal:Boolean) extends Literal{
    def doubleValue = if (literal) 1.0 else 0.0
    def boolValue   = literal
    def stringValue = literal.toString
    override def toString  = literal.toString
}

case class StringLiteral(s:String) extends Literal{
    val literal = s.substring(1,s.length-1)//TODO: apply missing escapes
    def doubleValue = literal.toDouble
    def boolValue   = if (literal.toLowerCase == "false") false else true
    def stringValue = literal
    override def toString  = s
}

case class Variable(name:String) extends Expression {
    def eval(env:Map[String,Literal]) = env(name)
    override def toString = name
}

case class MethodCall(name:String, params:List[Expression]) extends Expression {
    val method:(List[Literal] => Literal) = Methods.methods(name)
    def eval(env:Map[String, Literal]) = method(params.map(_.eval(env)))
    override def toString = name + "(" + params.mkString(",") + ")"
}

case class UnaryOp(val op:String, val arg:Expression) extends Expression {
    val opFunction:(Literal=>Literal) = Methods.unaryOperators(op)
    def eval(env:Map[String, Literal]) = opFunction(arg.eval(env))
    override def toString = op + "(" + arg + ")"
}

case class BinaryOp(val op:String, val arg1:Expression, val arg2:Expression) extends Expression {
    val opFunction:((Literal,Literal)=>Literal) = Methods.binaryOperators(op)
    def eval(env:Map[String, Literal]) = opFunction(arg1.eval(env), arg2.eval(env))
    override def toString = "(" + arg1 + " " + op + " " + arg2 + ")"
}

case class TernaryOp(val arg1:Expression, val arg2:Expression, val arg3:Expression) extends Expression {
    def eval(env:Map[String, Literal]) =  if (arg1.eval(env).boolValue) arg2.eval(env) else (arg3.eval(env))
    override def toString = "(" + arg1 + " ? " + arg2 + " : " + arg3 + ")"
}

object Methods {

    implicit def literalToBool  (l:Literal) = l.boolValue
    implicit def literalToDouble(l:Literal) = l.doubleValue
    implicit def literalToString(l:Literal) = l.stringValue

    implicit def boolToLiteral(b:Boolean)   = new BooleanLiteral(b)
    implicit def numberToLiteral (d:Double) = new NumberLiteral(d)
    implicit def stringToLiteral (s:String) = new StringLiteral("\"" + s + "\"")

    val methods = Map[String,(List[Literal]=>Literal)](
        "sin"  -> {case a :: _ => scala.math.sin(a)},
        "cos"  -> {case a :: _ => scala.math.cos(a)},
        "tan"  -> {case a :: _ => scala.math.tan(a)},
        "exp"  -> {case a :: _ => scala.math.exp(a)},
        "log"  -> {case a :: _ => scala.math.log(a)},
        "abs"  -> {case a :: _ => scala.math.abs(a)},
        "sqrt" -> {case a :: _ => scala.math.sqrt(a)},
        "signum" -> {case a :: _ => scala.math.signum(a)},
        "max"  -> {case a :: b :: _ => scala.math.max(a, b)},
        "min"  -> {case a :: b :: _ => scala.math.max(a, b)}
    )

    val unaryOperators = Map[String, (Literal=>Literal)]("!" -> (!_), "-" -> (0 - _), "+" ->(x => x))

    val binaryOperators = Map[String, ((Literal,Literal)=>Literal)](
        "*" -> (_ * _), "/" -> (_ / _), "%" -> (_.doubleValue.toInt % _.doubleValue.toInt),
        "+" -> (_ + _), "-" -> (_ - _),
        "<" -> (_ < _), ">" -> (_ > _),
        "<=" -> (_ <= _), ">=" -> (_ >= _),
        "==" -> (_.stringValue == _.stringValue), "!=" -> (_.stringValue != _.stringValue),
        "&&" -> (_ && _), "||" -> (_ || _),
        "++" -> (_.stringValue + _.stringValue)
    )

    val divide:PartialFunction[Int,Int] = {case x if (x!=0) => 42/x}

}
