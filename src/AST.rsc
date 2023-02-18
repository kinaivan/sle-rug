module AST

/*
 * Define Abstract Syntax for QL
 *
 * - complete the following data types
 * - make sure there is an almost one-to-one correspondence with the grammar
 */

data AForm(loc src = |tmp:///|)
  = form(str name, list[AQuestion] aQuestions)
;

data AQuestion(loc src = |tmp:///|)
  = question(str questionQuery, AId varName, AType typee)
  | compQuestion(str questionQuery, AId varName, AType typee, AExpr expr)
  | block(list[AQuestion] aQuestions)
  | ifElse(AExpr expr, list[AQuestion] aQuestions, list[AQuestion] aQuestions2)
  | ifThen(AExpr expr, list[AQuestion] aQuestions)
;

data AExpr(loc src = |tmp:///|)
  = ref(AId id)
  | integer(int integer)
  | boolean(bool tralse)
  | string(str string)
  | add(AExpr left, AExpr right)
  | sub(AExpr left, AExpr right)  
  | mul(AExpr left, AExpr right)
  | div(AExpr left, AExpr right)
  | eq(AExpr left, AExpr right)
  | gt(AExpr left, AExpr right)
  | lt(AExpr left, AExpr right)
  | geq(AExpr left, AExpr right)
  | leq(AExpr left, AExpr right)
  | neq(AExpr left, AExpr right)
  | not(AExpr left)
  | and(AExpr left, AExpr right)
  | or(AExpr left, AExpr right)
  | par(AExpr left)
;


data AId(loc src = |tmp:///|)
  = id(str name)
;


data AType(loc src = |tmp:///|)
  = string()
  | boolean()
  | integer()
;