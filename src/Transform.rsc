module Transform

import IO;
import Syntax;
import Resolve;
import AST;
import CST2AST;

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
  list[AQuestion] flattenedQuestions = [];
  for(q <- f.questions){
    flattenedQuestions += flatten(q, val(Boolean()));
  }
  f.questions = flattenedQuestions;
  return f; 
}

list[AQuestion] flatten(AQuestion q, AExpr condition){
  switch(q){
    case singleQuestion(_, _, _): return [(ifExprQuestion(condition, q))];
    case computedQuestion(_, _, _, _): return [ifExprQuestion(condition, q)];
    case block(questions): {
      list[AQuestion] result = [];
      for (question <- questions){
        result += [ifExprQuestion(condition, question)];
      }
      return result;
    }
    case ifElseQuestion(AId conditionId, ifQuestions, elseQuestions): {
      list[AQuestion] result = [];
      for(question <- ifQuestions){
        result += flatten(question, addCondition(true, ref(conditionId), condition));
      }
      for(question <- elseQuestions) {
        result += flatten(question, addCondition(false, ref(conditionId), condition));
      }
      return result;
    }
    case ifQuestion(AId conditionId, ifQuestions): {
      list[AQuestion] result = [];
      for(question <- ifQuestions){
        result += flatten(question, addCondition(true, ref(conditionId), condition));
      }
      return result;
    }
    default: throw "unimplemented question <q>";
  }
}

AExpr addCondition(bool mustBeTrue, AExpr exprToAdd, AExpr currentExpression){
  if (!mustBeTrue){
    exprToAdd = negatedExpr(not(), exprToAdd);
  }
  return computationExpr(currentExpression, "&&", exprToAdd);
}

void testFlatten(){
  AForm f = getExampleAST();
  f = flatten(f);
  for(q <- f.questions){
      println("if(<q.e>): <q.singleQ.label>");
  }
}

/* Rename refactoring:
 *
 * Write a refactoring transformation that consistently renames all occurrences of the same name.
 * Use the results of name resolution to find the equivalence class of a name.
 *
 */
 
start[Form] rename(start[Form] f, loc useOrDef, str newName, UseDef useDef) {
  loc definition;
  for(ud <- useDef){
    if (ud.use == useOrDef || ud.def == useOrDef){
      definition = ud.def;
      break;
    }
    throw "no definition found for <useOrDef>";
  }
  for (use <- {x.use | x <- useDef && x.def == definition}){
    writeFile(use, newName);
  }
  writeFile(definition, newName);
  // I tried using the useOrDef path to re-parse instead of example(), but I could not find a way to 
  // use the string given by useOrDef.path and turn it into a location to be used by readFile().
  // All examples in the documentation use direclty typed strings, no variables.
  return example();
} 

void testRename(){
  start[Form] f = example();
  AForm Af = cst2ast(f);
  RefGraph r = resolve(Af);
  AQuestion q = Af.questions[2];
  rename(f, q.id.src, "myNewName", r.useDef);
}
 
 
 

