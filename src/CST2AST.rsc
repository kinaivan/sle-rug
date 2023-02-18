module CST2AST

import Syntax;
import AST;
import String;

/*
 * Implement a mapping from concrete syntax trees (CSTs) to abstract syntax trees (ASTs)
 *
 * - Use switch to do case distinction with concrete patterns (like in Hack your JS) 
 * - Map regular CST arguments (e.g., *, +, ?) to lists 
 *   (NB: you can iterate over * / + arguments using `<-` in comprehensions or for-loops).
 * - Map lexical nodes to Rascal primitive types (bool, int, str)
 * - See the ref example on how to obtain and propagate source locations.
 */

AForm cst2ast(start[Form] sf) {
  Form f = sf.top; // remove layout before and after form
  return form(f.name, [cst2ast(fo) | fo <- f.aQuestions], src=f.src);
}

AForm cst2ast(f:(Form)`form <Id name> { <Question* fs> }`) {
  return form(cst2ast(name), [cst2ast(fo) | fo <- fs], src=f.src);
}

default AQuestion cst2ast(f:(Question)`<Str text> <Id var> : <Type typee>`) {
  return question(cst2ast(text), cst2ast(var), cst2ast(typee), src=f.src);
}

AQuestion cst2ast(f:(Question)`<Str text> <Id var> : <Type typee> = <Expr expr>`) {
  return compQuestion(cst2ast(text), cst2ast(var), cst2ast(typee), cst2ast(expr), src=f.src);
}

AQuestion cst2ast(f:(Question)`if ( <Expr expr> ) { <Question* fs1> } else { <Question* fs2> }`) {
  return ifElse(cst2ast(expr), [cst2ast(fo1) | fo1 <- fs1], [cst2ast(fo2) | Question fo2 <- fs2], src=f.src);
}

AQuestion cst2ast(f:(Question)`if  ( <Expr expr> ) { <Question* fs> }`) {
  return ifThen(cst2ast(expr), [cst2ast(fo) | fo <- fs], src=f.src);
}

AExpr cst2ast(Expr e) {
  switch (e) {
    case (Expr)`<Id x>`: return ref(id("<x>", src=x.src), src=x.src);
    case (Expr)`<Int intege>`: return integer(toInt("<intege>"), src=integer.src);
    case (Expr)`<Bool b>`: return boolean(fromString("<b>"), src = b.src);
    case (Expr)`(<Expr expr>)`: return par(cst2ast(expr), src=expr.src);
    case (Expr)`!<Expr expr>` : return not(cst2ast(expr), src=expr.src);
    case (Expr)`<Expr lhs> + <Expr rhs>` : return add(cst2ast(lhs), cst2ast(rhs), src=lhs.src);
    case (Expr)`<Expr lhs> - <Expr rhs>` : return sub(cst2ast(lhs), cst2ast(rhs), src=lhs.src);
    case (Expr)`<Expr lhs> * <Expr rhs>` : return mul(cst2ast(lhs), cst2ast(rhs), src=lhs.src);
    case (Expr)`<Expr lhs> / <Expr rhs>` : return div(cst2ast(lhs), cst2ast(rhs), src=lhs.src);
    case (Expr)`<Expr lhs> == <Expr rhs>`: return eq(cst2ast(lhs), cst2ast(rhs), src=lhs.src);
    case (Expr)`<Expr lhs> != <Expr rhs>`: return neq(cst2ast(lhs), cst2ast(rhs), src=lhs.src);
    case (Expr)`<Expr lhs> \< <Expr rhs>`: return lt(cst2ast(lhs), cst2ast(rhs), src=lhs.src);
    case (Expr)`<Expr lhs> \> <Expr rhs>`: return gt(cst2ast(lhs), cst2ast(rhs), src=lhs.src);
    case (Expr)`<Expr lhs> \<= <Expr rhs>`: return leq(cst2ast(lhs), cst2ast(rhs), src=lhs.src);
    case (Expr)`<Expr lhs> \>= <Expr rhs>`: return geq(cst2ast(lhs), cst2ast(rhs), src=lhs.src);
    case (Expr)`<Expr lhs> && <Expr rhs>`: return and(cst2ast(lhs), cst2ast(rhs), src=lhs.src);
    case (Expr)`<Expr lhs> || <Expr rhs>`: return or(cst2ast(lhs), cst2ast(rhs), src=lhs.src);
    default: throw "Unhandled expression: <e>";
  }
}


AId cst2ast(Id i) {
  return id("<i>", src=i.src);
}


default AType cst2ast(Type t) {
  switch (t) {
    case (Type)`integer`: return integer(src=t.src);
    case (Type)`boolean`: return boolean(src=t.src);
    case (Type)`string` : return string(src=t.src);
    default: throw "Unhandled type: <t>";
  }
}