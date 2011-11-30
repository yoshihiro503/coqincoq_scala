import scala.util.parsing.combinator._

import CoqMain.{REF,APP,ABS,PROD,SRT, Expr}
import CoqMain.{Prop,Set,Kind, Sort}
import CoqMain.{AST_AXIOM, AST_CHECK, Ast}

class AstParser extends RegexParsers {
  override def skipWhitespace = true

  def is_ident_char(c: Char) = ('a' to 'z' contains c) ||
  ('A' to 'Z' contains c) ||('0' to '9' contains c) || c == '_' || c == '\''
  
  def ident = rep1 (elem("ident1", is_ident_char)) ^^ (_.mkString) flatMap {
    case "let" => failure("let")
    case "in" => failure("in")
    case x => success(x)
  }
  
  def white = rep1 (elem("white1", c => c==' ' || c=='\t' || c=='\n'))

  def rep1_[A](p : Parser[A]) =
    rep1(p) ^^ {case(x::xs) => (x,xs); case List() =>
      throw new Exception("must not happen")}
  def rep1sep_[A](p : Parser[A], q : Parser[Any]) =
    rep1sep(p,q) ^^ {case(x::xs) => (x,xs); case List() =>
      throw new Exception("must not happen")}

  def expr : Parser[Expr] = rep1sep_(expr1, white) ^^
    {case (e, es) => es.foldLeft[Expr](e)(APP(_,_))}

  def expr1 : Parser[Expr] = rep1sep(expr2, "->") ^^
    {es => es.reduceRight[Expr](PROD("_", _, _))}

  def expr2 : Parser[Expr] =
    "Prop" ~> success(SRT(Prop())) |
    "Set" ~> success(SRT(Set())) |
    "Kind" ~> success(SRT(Kind())) |
    abs  ^^ {case (xs,ty,t) => xs.foldRight(t)(ABS(_,ty,_))} |
    prod ^^ {case (xs,ty,t) => xs.foldRight(t)(PROD(_,ty,_))} |
    let ^^  {case (x,ty,a,t) => APP(ABS(x,ty,t), a)} |
    ident ^^ (s => REF (s)) |
    "(" ~> expr <~ ")" 
    
  def abs : Parser[(List[String], Expr, Expr)] = for {
    _ <- literal("[") <~ opt(white)
    xs <- rep1sep(ident, ",")
    _ <- literal(":") <~ opt(white)
    ty <- expr
    _ <- literal("]") <~ opt(white)
    t <- expr1
  } yield (xs, ty, t)

  def prod : Parser[(List[String], Expr, Expr)] = for {
    _ <- literal("(") <~ opt(white)
    xs <- rep1sep(ident, ",")
    _ <- literal(":") <~ opt(white)
    ty <- expr
    _ <- literal(")") <~ opt(white)
    t <- expr1
  } yield (xs, ty, t)

  def let : Parser[(String, Expr, Expr, Expr)] = for {
    _ <- literal("let") <~ opt(white)
    x <- ident
    _ <- literal(":") <~ opt(white)
    ty <- expr
    _ <- literal(":=") <~ opt(white)
    a <- expr
    _ <- literal("in") <~ opt(white)
    t <- expr
  } yield (x, ty, a, t)





  def ast : Parser[Ast] =
    axiom ^^ {case (x, ty) => AST_AXIOM(x, ty)} |
    check ^^ {case (t, ty) => AST_CHECK(t, ty)}

  def axiom : Parser[(String, Expr)] = for {
    _ <- literal("Axiom") <~ opt(white)
    x <- ident
    _ <- literal (":") <~ opt(white)
    ty <- expr
    _ <- literal (".")
  } yield {(x, ty)}

  def check : Parser[(Expr, Expr)] = for {
    _ <- literal("Check") <~ opt(white)
    t <- expr
    _ <- literal(":") <~ opt(white)
    ty <- expr
    _ <- literal (".")
  } yield {(t, ty)}

  def asts = rep(ast)

}