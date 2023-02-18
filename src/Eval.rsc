module Eval

import AST;
import Resolve;

/*
 * Implement big-step semantics for QL
 */
 
// NB: Eval may assume the form is type- and name-correct.


// Semantic domain for expressions (values)
data Value
  = vint(int n)
  | vbool(bool b)
  | vstr(str s)
  ;

// The value environment
alias VEnv = map[str name, Value \value];

// Modeling user input
data Input
  = input(str question, Value \value);
  
// produce an environment which for each question has a default value
// (e.g. 0 for int, "" for str etc.)
VEnv initialEnv(AForm f) {
  VEnv initv = ();
  visit(f) {
    case question(_, AId id, AType t) : 
    switch(t){
      case integer(): initv += (id.name: vint(0));
      case boolean(): initv += (id.name: vbool(false));
      case string(): initv += (id.name: vstr(""));
    }
    case compQuestion(_, AId id, AType t, _):
    switch(t){
      case integer(): initv += (id.name: vint(0));
      case boolean(): initv += (id.name: vbool(false));
      case string(): initv += (id.name: vstr(""));
    }
  }
  return initv;
}


// Because of out-of-order use and declaration of questions
// we use the solve primitive in Rascal to find the fixpoint of venv.
VEnv eval(AForm f, Input inp, VEnv venv) {
  return solve (venv) {
    venv = evalOnce(f, inp, venv);
  }
}

VEnv evalOnce(AForm f, Input inp, VEnv venv) {
  for ( q <- f.aQuestions) {
    venv = eval(q, inp, venv);
  }
  return venv;
}

VEnv eval(AQuestion q, Input inp, VEnv venv) {
  // evaluate conditions for branching,
  // evaluate inp and computed questions to return updated VEnv
  switch(q) {
    case question(str _, AId varName, AType typee): 
      if (varName.name == inp.question) {
        venv += (inp.question: inp.\value);
      }
    case compQuestion(str _, AId varName, AType typee, AExpr expr):
      venv[varName.name] = eval(expr, venv);
    case block(list[AQuestion] aQuestions):
      for (qs <- aQuestions){
        venv = eval(qs, inp, venv);
      }
    case ifElse(AExpr expr, list[AQuestion] aQuestions, list[AQuestion] aQuestions2): 
      if (eval(expr, venv).b) {
        for (qs <- aQuestions) {
          venv = eval(qs, inp, venv);
        }
      } else {
        for (qs <- aQuestions2) {
          venv = eval(qs, inp, venv);
        }        
      }
    case ifThen(AExpr expr, list[AQuestion] aQuestions): 
      if (eval(expr, venv).b) {
        for (qs <- aQuestions) {
          venv = eval(qs, inp, venv);
        }
      }
  }
  return venv; 
}

Value eval(AExpr e, VEnv venv) {
  switch (e) {
    case ref(id(str x)): return venv[x];
    case boolean(bool tralse): return vbool(tralse);
    case integer(int i): return vint(i);
    case string(str s): return vstr(s);
    case add(AExpr left, AExpr right): return vint(eval(left,venv).n + eval(right, venv).n);
    case sub(AExpr left, AExpr right): return vint(eval(left,venv).n - eval(right, venv).n);
    case mul(AExpr left, AExpr right): return vint(eval(left,venv).n * eval(right, venv).n);
    case div(AExpr left, AExpr right): return vint(eval(left,venv).n / eval(right, venv).n);
    case eq(AExpr left, AExpr right): return vbool(eval(left,venv).n == eval(right, venv).n);
    case gt(AExpr left, AExpr right): return vbool(eval(left,venv).n > eval(right, venv).n);
    case lt(AExpr left, AExpr right): return vbool(eval(left,venv).n < eval(right, venv).n);
    case geq(AExpr left, AExpr right): return vbool(eval(left,venv).n >= eval(right, venv).n); 
    case leq(AExpr left, AExpr right): return vbool(eval(left,venv).n <= eval(right, venv).n); 
    case neq(AExpr left, AExpr right): return vbool(eval(left,venv).n != eval(right, venv).n);
    case not(AExpr left): return vbool(!eval(left,venv).b);
    case and(AExpr left, AExpr right): return vbool(eval(left,venv).b && eval(right, venv).b); 
    case or(AExpr left, AExpr right): return vbool(eval(left,venv).b || eval(right, venv).b);
    case par(AExpr left): return eval(left, venv);

    default: throw "Unsupported expression <e>";
  }
}