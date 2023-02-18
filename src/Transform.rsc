module Transform

import Syntax;
import Resolve;
import AST;

/* 
 * Transforming QL forms
 */
 
 
/* Normalization:
 *  wrt to the semantics of QL the following
 *     q0: "" int; 
 *     if (a) { 
 *        if (b) { 
 *          q1: "" int; 
 *        } 
 *        q2: "" int; 
 *      }
 *
 *  is equivalent to
 *     if (true) q0: "" int;
 *     if (true && a && b) q1: "" int;
 *     if (true && a) q2: "" int;
 *
 * Write a transformation that performs this flattening transformation.
 *
 */
 
AForm flatten(AForm f) {
  f.aQuestions = flattenQs(f.aQuestions, boolean(false));
  return f; 
}

list[AQuestion] flattenQs(list[AQuestion] aQuestions, AExpr boolean) {
  list[AQuestion] questions = [];
  for (q <- aQuestions){
    questions += flattenQ(q, boolean);
  }
  return questions;
}

list[AQuestion] flattenQ(AQuestion question, AExpr boolean){
  list[AQuestion] questions = [];
  switch(question) {
    case question(str _, AId varName, AType typee): 
      questions += ifThen(boolean, [question]);
    case compQuestion(str _, AId varName, AType typee, AExpr expr):
      questions += ifThen(boolean, [question]);
    case block(list[AQuestion] aQuestions): 
      for (question2 <- aQuestions){
        questions += flattenQ(question2, boolean);
      }
    case ifElse(AExpr expr, list[AQuestion] aQuestions, list[AQuestion] aQuestions2):{
      for (question2 <- aQuestions){
        questions += flattenQ(question2, and(expr, boolean));
      } 
      for (question2 <- aQuestions2){
        questions += flattenQ(question2, and(not(expr), boolean));
      }
    }
    case ifThen(AExpr expr, list[AQuestion] aQuestions): 
      for (question2 <- aQuestions){
        questions += flattenQ(question2, and(expr, boolean));
      }
  }
  return questions;
}

/* Rename refactoring:
 *
 * Write a refactoring transformation that consistently renames all occurrences of the same name.
 * Use the results of name resolution to find the equivalence class of a name.
 *
 */
 
start[Form] rename(start[Form] f, loc useOrDef, str newName, UseDef useDef) {

  set[loc] toRename = {};

  if(useOrDef in useDef<0>){
    toRename += {useOrDef};
    toRename += {use | <loc use, useOrDef> <- useDef};
  }else if(useOrDef in useDef<1>){
    if(<useOrDef, loc def> <- useDef){
      toRename += def;
      toRename += {use | <loc use, def> <- useDef};
    }
  }else{
    return f;
  }

  return visit (f) {
    case Id x => [Id]newName
      when x.src in toRename
  }
} 

 
 

