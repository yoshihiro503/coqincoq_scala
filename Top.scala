import scala.io._
import CoqMain._

object Top {

  def print_newline () = println ("")

  def string_of_sort : Sort => String = {
    case Kind() => "Kind"
    case Prop() => "Prop"
    case Set() => "Set"
  }

  def string_of_expr : Expr => String = {
    case SRT (s) => string_of_sort (s)
    case REF (x) => x
    case ABS (x,tt,t) => "["+x+":"+string_of_expr(tt)+"]"+(string_of_expr (t))
    case APP (u,v) => "("+(string_of_app (u))+" "+(string_of_expr (v))+")"
    case PROD (x,tt,u) =>
      is_free_var (x) (u) match {
       case Left() => "("+x+":"+(string_of_expr (tt))+")"+(string_of_expr (u))
       case Right() => (string_of_arrow (tt))+"=>"+(string_of_expr (u))
      }
  }

  def string_of_app : Expr => String = {
    case APP (u,v) => (string_of_app (u))+" "+(string_of_expr (v))
    case t => string_of_expr (t)
  }

  def string_of_arrow : Expr => String = {
    case ABS (x0,x1,x2) => "("+(string_of_expr (ABS (x0,x1,x2)))+")"
    case PROD (x0,x1,x2) => "("+(string_of_expr (PROD (x0,x1,x2)))+")"
    case t => string_of_expr (t)
  }

  def print_expr (e: Expr) = print(string_of_expr (e));;


  def print_names : List[Name] => Unit = {
    case Nil() => ()
    case Cons (x,l) =>
      print_names (l);
      print (x+" ")
  }

  def print_message : Pmessage => Unit = {
    case Pnew_axiom (x) =>
      println (x+" admis.")
    case Pinfered_type (e) =>
      print ("Type infere: ");
      print_expr (e)
      print_newline()
    case Pcorrect() =>
      println ("Correct.")
    case Pcontext_listing (l) =>
      print ("Axiomes: ");
      print_names (l);
      print_newline()
    case Pdelete_axiom (x) =>
      println (x + " supprime.")
    case Pexiting() =>
      println ("\nAu revoir..."); sys.exit (0)
  }

  def print_type_err : Ptype_error => Unit = {
    case Punder (x,e,err) =>
      print (x);
      print (" : ");
      print_expr (e);
      print_newline();
      print_type_err (err)
    case Pexpected_type(m,at,et) =>
      print ("Le terme ");
      print_expr (m);
      print (" a le type ");
      print_expr (at);
      print (" mais est utilise avec le type ");
      print_expr (et);
      println (".")
  case Pkind_ill_typed() =>
      println ("Kind est mal type.")
  case Pdb_error (n) =>
      print ("Variable de de Bruijn #");
      print (int_of_nat (n))
      println (" libre.")
  case Plambda_kind (t) =>
      print ("Le terme ");
      print_expr (t);
      println (" est une abstraction sur une kind.")
  case Pnot_a_type(m,t) =>
      print ("Le type de ")
      print_expr (m)
      print (", qui est ")
      print_expr (t)
      println (" ne se reduit pas vers une sorte.")
  case Pnot_a_fun(m,t) =>
      print ("Le type de ")
      print_expr (m)
      print (", qui est ")
      print_expr (t)
      println (" ne se reduit pas vers un produit.")
  case Papply_err(u,tu,v,tv) =>
      print ("Le terme ");
      print_expr (u)
      print (" de type ");
      print_expr (tu);
      print (" ne peut etre applique a ");
      print_expr (v);
      print (" qui a pour type ");
      print_expr( tv);
      println (".")
  }

  def print_type_error (err: Ptype_error) = {
    err match {
      case Punder (_,_,_) =>
        println ("Dans le contexte:");
      case _ => ()
    }
    print_type_err (err)
  }

  def print_error : Perror => Unit = {
    case Punbound_vars (l) =>
      print ("Variables inconnues: [ ");
      print_names (l);
      println ("].")
    case Pname_clash (x) =>
      println ("Nom " +x+ " deja utilise.")
    case Ptype_error0 (te) =>
      print_type_error (te)
    case Pcannot_delete() =>
      println ("Contexte deja vide.")
  }

  def update_state = {
    var state = empty_state
    ((ast: Ast) =>
     interp_ast(state)(ast) match {
       case Inl(Pair(ns,msg)) =>
             print_message (msg);
             state = ns
       case Inr(err) =>
             print ("Erreur: ");
             print_error (err)
     })
  }


  def parse(src : Source) = {
    import scala.util.parsing.input._
    val reader = new CharSequenceReader(src.mkString)
    (new AstParser).asts(reader).get
  }

  def go () = {
    val asts = parse(Source.stdin)
    asts.foreach(update_state)
    println ("EOF!")
  }

  def main(args : Array[String]) = {
    go()
  }
}
