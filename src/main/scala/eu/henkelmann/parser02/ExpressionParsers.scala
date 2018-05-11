package eu.henkelmann.parser02

package eu.henkelmann.lgenerator.parser

import scala.util.parsing.combinator.JavaTokenParsers

/**
 * Parser for expressions
 * a simplified version of the java expressions as described in
 * chapter 15 of the java language spec
 */

trait ExpressionParser extends JavaTokenParsers {

    def methodName:Parser[String]             = ident
    def argumentList:Parser[List[Expression]] = repsep(expression, ",")
    def methodCall:Parser[Expression]   = methodName ~ "(" ~ (argumentList) ~ ")" ^^ {
        case s ~ _ ~ args ~ _ => new MethodCall(s, args)
    }

    def boolean :Parser[Expression] = ("true" | "false")  ^^ {s => new BooleanLiteral(s.toBoolean)}
    def string  :Parser[Expression] = super.stringLiteral ^^ {s => new StringLiteral(s)}
    def double  :Parser[Expression] = (decimalNumber | floatingPointNumber) ^^ {s => new NumberLiteral(s.toDouble)}
    def int     :Parser[Expression] = wholeNumber         ^^ {s => new NumberLiteral(s.toInt)}
    def literal :Parser[Expression] = boolean | string | double | int

    def variable:Parser[Expression] = ident ^^ {s => new Variable(s)}

    def primary:Parser[Expression] = literal | ("(" ~> expression <~ ")") | methodCall | variable

    def unary:Parser[Expression] = primary | (("!"|"+"|"-") ~ unary)^^{case op ~ e => new UnaryOp(op,e)}

    def multiplication:Parser[Expression] = (unary ~ rep(("*"|"/"|"%") ~ unary))^^{foldExpressions}

    def addition:Parser[Expression] = (multiplication ~ rep(("+"|"-") ~ multiplication))^^{foldExpressions}

    def relational:Parser[Expression] = (addition ~ rep(("<=" | ">=" | "<" | ">") ~ addition))^^{foldExpressions}

    def equality:Parser[Expression] = (relational ~ rep(("=="|"!=") ~ relational))^^{foldExpressions}

    def and:Parser[Expression] = (equality ~ rep("&&" ~ equality))^^{foldExpressions}

    def or:Parser[Expression] = (and ~ rep("||" ~ and))^^{foldExpressions}

    def conditional:Parser[Expression] = (or ~ "?" ~ expression ~ ":" ~ conditional)^^{case e1 ~ _ ~ e2 ~ _ ~ e3=> new TernaryOp(e1, e2, e3)} | or

    def expression:Parser[Expression] = conditional

    def foldExpressions(result:ExpressionParser.this.~[Expression,List[ExpressionParser.this.~[String,Expression]]]) = result match {
        case e ~ list => list.foldLeft(e) ((exp,el)=> new BinaryOp(el._1, exp, el._2))
    }
}