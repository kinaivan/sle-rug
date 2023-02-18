module Syntax
import IO;

extend lang::std::Layout;
extend lang::std::Id;

/*
 * Concrete syntax of QL
 */

start syntax Form 
  = "form" Id "{" Fork* "}"
; 

syntax Fork
 = QuestionChoice
 | Block 
 | IfThen
 | IfElse
;

syntax QuestionChoice
  = CompQuestion
  | Question
;

syntax IfThen
  = "if" "(" Expr ")" "{" Fork* "}"
;

syntax IfElse
  = "if" "(" Expr ")" "{" Fork* "}" "else" "{" Fork* "}"
;
  
// TODO: question, computed question, block, if-then-else, if-then
syntax Question
  = "\"" QuestionName "\"" Id ":" Type
;

syntax CompQuestion
  = "\"" QuestionName "\"" Id ":" Type "=" Expr
;

syntax Block
  = "{" QuestionChoice* "}"
;

// TODO: +, -, *, /, &&, ||, !, >, <, <=, >=, ==, !=, literals (bool, int, str)
// Think about disambiguation using priorities and associativity
// and use C/Java style precedence rules (look it up on the internet)

syntax Expr
  = var: Id \ "true" \ "false"
  | integer: Int
  | boolean: Bool
  > left div: Expr l "/" Expr r
  > left mul: Expr l "*" Expr r
  > left add: Expr l "+" Expr r
  > left sub: Expr l "-" Expr r
  > left gt: Expr "\>" Expr
  > left lt: Expr "\<" Expr
  > left geq: Expr "\>=" Expr
  > left leq: Expr "\<=" Expr
  > left neq: Expr "!=" Expr
  > left not: "!" Expr
  > left and: Expr "||" Expr
  > left or: Expr "&&" Expr
  > bracket "(" Expr ")"  
;

  
syntax Type = "string" | "integer" | "boolean";

lexical Str = [A-Za-z ]*;

lexical Int = [0-9]*;

lexical Bool = "true" | "false";

lexical QuestionName = [a-zA-z][A-Z a-z0-9]*[?|:];