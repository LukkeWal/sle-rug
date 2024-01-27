module Transform

import IO;
import Syntax;
import Resolve;
import AST;
import CST2AST;
import Set;

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
    flattenedQuestions += flatten(q, val(Boolean(true)));
  }
  f.questions = flattenedQuestions;
  return f; 
}

list[AQuestion] flatten(AQuestion q, AExpr condition){
  switch(q){
    case singleQuestion(_, _, _): return [(ifQuestion(condition, q))];
    case computedQuestion(_, _, _, _): return [ifQuestion(condition, q)];
    case block(questions): {
      list[AQuestion] result = [];
      for (question <- questions){
        result += flatten(question, condition);
      }
      return result;
    }
    case ifElseQuestion(AExpr guard, ifQuestion, elseQuestion): {
      list[AQuestion] result = [];
      result += flatten(ifQuestion, addCondition(true, guard, condition));
      result += flatten(elseQuestion, addCondition(false, guard, condition));
      return result;
    }
    case ifQuestion(AExpr guard, ifQuestion): return flatten(ifQuestion, addCondition(true, guard, condition));
    default: throw "unimplemented question <q>";
  }
}

AExpr addCondition(bool mustBeTrue, AExpr exprToAdd, AExpr currentExpression){
  if (!mustBeTrue){
    exprToAdd = negation("!", exprToAdd);
  }
  return computation(currentExpression, "&&", exprToAdd);
}

test bool testFlattenTax(){
  AForm f = getAST(5);
  AForm result = flatten(f);
  for(q <- result.questions){
    if (!(q is ifQuestion)){
      return false;
    }
  }
  return true;
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
  loc fileLocation = |file:///| + "<useOrDef.path>";
  return getForm(fileLocation);
}

test bool testRenameTax(){
  start[Form] f = getForm(5);
  RefGraph r = resolve(cst2ast(f));
  start[Form] result;
  if(r.defs["hasSoldHouse"] == {}){
    return false;
  }
  for(def <- r.defs["hasSoldHouse"]){
    result = rename(f, def, "NewName", r.useDef);
    break;
  }
  AForm Aresult = cst2ast(result);
  RefGraph rResult = resolve(Aresult);
  if(rResult.defs["hasSoldHouse"] == {}){
    return true;
  }
  return false;
}
 
 
 

