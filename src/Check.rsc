module Check

import AST;
import Resolve;
import Message; // see standard library
import List;
import Set;

data Type
  = tint()
  | tbool()
  | tstr()
  | tunknown()
  ;

// the type environment consisting of defined questions in the form 
alias TEnv = rel[loc def, str name, str label, Type \type];

// To avoid recursively traversing the form, use the `visit` construct
// or deep match (e.g., `for (/question(...) := f) {...}` ) 
TEnv collect(AForm f) {
  TEnv returnValue1 = { <varName.src, varName.name, questionQuery, convert(\type)> | /question(str questionQuery, AId varName, AType \type) := f };
  TEnv returnValue2 = { <varName.src, varName.name, questionQuery, convert(\type)> | /compQuestion(str questionQuery, AId varName, AType \type, _) := f };
  return returnValue1 + returnValue2;
}


private Type convert(AType \type) {
	switch(\type) {
		case integer(): return tint();
		case string(): return tstr();
		default: return tbool();
	}
}

set[Message] check(AForm f, TEnv tenv, UseDef useDef) {
  set[Message] msgs = {};
  for (AQuestion question <- f.aQuestions) {
    msgs += check(question, tenv, useDef);
  }
  return msgs; 
}

// - produce an error if there are declared questions with the same name but different types.
// - duplicate labels should trigger a warning 
// - the declared type computed questions should match the type of the expression.
set[Message] check(AQuestion q, TEnv tenv, UseDef useDef) {
  set[Message] msgs = {};
  switch(q) {
  	case compQuestion(str questionQuery, AId varName, AType typee, AExpr expr): 
     msgs += (convert(typee) != typeOf(expr, tenv, useDef)) ? {} : {error("Error: Type of computed questions does not match the type of the expression", q.src)};
    case question(str questionQuery, AId varName, AType typee):
     msgs += (size(toList(tenv)[_, _, q.questionQuery]) != 0) ? {} : {error("Error: There are declared questions with the same name but different type")};
  }

  return {};
}

// Check operand compatibility with operators.
// E.g. for an addition node add(lhs, rhs), 
//   the requirement is that typeOf(lhs) == typeOf(rhs) == tint()
set[Message] check(AExpr e, TEnv tenv, UseDef useDef) {
  set[Message] msgs = {};
  
  switch (e) {
    case ref(AId x):
      msgs += { error("Error: Undeclared question", x.src) | useDef[x.src] == {} };
    case add(AExpr \left, AExpr \right):
      msgs += ((typeOf(left, tenv, useDef) == tint()) && (typeOf(right, tenv, useDef) == tint())) ? {} : {error("Error: Type is not int", e.src)};
    case sub(AExpr left, AExpr right):
      msgs += (typeOf(left, tenv, useDef) == tint() && typeOf(right, tenv, useDef) == tint()) ? {} : {error("Error: Type is not int", e.src)};
    case mul(AExpr left, AExpr right):
      msgs += (typeOf(left, tenv, useDef) == tint() && typeOf(right, tenv, useDef) == tint()) ? {} : {error("Error: Type is not int", e.src)};
    case div(AExpr left, AExpr right):
      msgs += (typeOf(left, tenv, useDef) == tint() && typeOf(right, tenv, useDef) == tint()) ? {} : {error("Error: Type is not int", e.src)};
    case eq(AExpr left, AExpr right):
      msgs += typeOf(left, tenv, useDef) == typeOf(right, tenv, useDef) ? {} : {error("Error: Unexpected, types are not the same", e.src)};
    case gt(AExpr left, AExpr right):
      msgs += (typeOf(left, tenv, useDef) == tbool() && typeOf(right, tenv, useDef) == tbool()) ? {} : {error("Error: Type is not bool", e.src)};
    case lt(AExpr left, AExpr right):
      msgs += (typeOf(left, tenv, useDef) == tbool() && typeOf(right, tenv, useDef) == tbool()) ? {} : {error("Error: Type is not bool", e.src)};
    case geq(AExpr left, AExpr right):
      msgs += (typeOf(left, tenv, useDef) == tbool() && typeOf(right, tenv, useDef) == tbool()) ? {} : {error("Error: Type is not bool", e.src)};
    case leq(AExpr left, AExpr right):
      msgs += (typeOf(left, tenv, useDef) == tbool() || typeOf(right, tenv, useDef) == tbool()) ? {} : {error("Error: Type is not bool", e.src)};
    case neq(AExpr left, AExpr right):
      msgs += typeOf(left, tenv, useDef) == typeOf(right, tenv, useDef) ? {} : {error("Error: Unexpected, types are not the same", e.src)};
    case not(AExpr left):
      msgs += typeOf(left, tenv, useDef) == typeOf(left, tenv, useDef) ? {} : {error("Error: Unexpected, types are not the same", e.src)};
    case and(AExpr left, AExpr right):
      msgs += (typeOf(left, tenv, useDef) == tbool() || typeOf(right, tenv, useDef) == tbool()) ? {} : {error("Error: Error: Type is not bool", e.src)};
    case or(AExpr left, AExpr right):
      msgs += (typeOf(left, tenv, useDef) == tbool() || typeOf(right, tenv, useDef) == tbool()) ? {} : {error("Error: Type is not bool", e.src)};
    case par(AExpr x):
      msgs += check(x, tenv, useDef);
    default : {};
    }
  return msgs; 
}

Type typeOf(AExpr e, TEnv tenv, UseDef useDef) {
  switch (e) {
    case ref(id(_, src = loc u)):  
      if (<u, loc d> <- useDef, <d, x, _, Type t> <- tenv) {
        return t;
      }
    case add(_, _): return tint();
    case sub(_, _): return tint();
    case mul(_, _): return tint();
    case div(_, _): return tint();
    case eq(_, _): return tbool();
    case gt(_, _):  return tbool();
    case lt(_, _): return tbool();
    case geq(_, _): return tbool();
    case leq(_, _): return tbool();
    case neq(_, _): return tbool();
    case and(_, _): return tbool();
    case or(_, _): return tbool();
    case not(_):    return tbool();

  }
  return tunknown(); 
}

/* 
 * Pattern-based dispatch style:
 * 
 * Type typeOf(ref(id(_, src = loc u)), TEnv tenv, UseDef useDef) = t
 *   when <u, loc d> <- useDef, <d, x, _, Type t> <- tenv
 *
 * ... etc.
 * 
 * default Type typeOf(AExpr _, TEnv _, UseDef _) = tunknown();
 *
 */
 
 

