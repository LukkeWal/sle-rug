module CST2AST
import IO;
import Syntax;
import AST;

import ParseTree;

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
  result = form("<f.name>", [cst2ast(q) | q <- f.questions], src=f.src);
  return result;
}

default AQuestion cst2ast(Question q) {
  switch(q){
    case singleQuestion(Str label,Id id,Type finaltype):
      return singleQuestion("<label>", cst2ast(id), cst2ast(finaltype), src=q.src);
    case computedQuestion(label, Id id, Type finaltype, Expr expression):
      return computedQuestion("<label>", cst2ast(id), cst2ast(finaltype), cst2ast(expression), src=q.src);
    case block(questions):
      return block([cst2ast(x) | x <- questions], src=q.src);
    case ifQuestion(conditionId, ifQuestions):
      return ifQuestion(cst2ast(conditionId), [cst2ast(x) | x <- ifQuestions], src=q.src);
    case ifElseQuestion(conditionId, ifQuestions, elseQuestions): 
      return ifElseQuestion(cst2ast(conditionId), [cst2ast(x) | x <- ifQuestions], [cst2ast(x) | x <- elseQuestions], src=q.src);
    default: throw "Unhandled question: <q>";
  }
}

AExpr cst2ast(Expr e) {
  switch (e) {
    case (Expr)`<Id x>`: return ref(id("<x>", src=x.src), src=x.src);
    case Computation(a, operator, b): return computationExpr(cst2ast(a), "<operator>", cst2ast(b), src=e.src);
    case val(v): return cst2ast(v, src=e.src);
    default: throw "Unhandled expression: <e>";
  }
}

AValue cst2ast(Value v){
  switch(v){
    case string(): return String(src=v.src);
    case integer(): return Boolean(src=v.src);
    case boolean(): return Integer(src=v.src);
    default: throw "Unhandled value: <v>";
  }
}


default AType cst2ast(Type t) {
  switch(t){
    case String(): return stringType(src = t.src);
    case Boolean(): return booleanType(src = t.src);
    case Integer(): return integerType(src = t.src);
    default: throw "Unhandled type: <t>"; 
  }
}

AId cst2ast(Id i){
  return id("<i>", src=i.src);
}

AForm getExampleAST(){
  start[Form] exampleForm = example();
  return cst2ast(exampleForm);
}

void testImplementation(){
  start[Form] exampleForm = example();
  AForm result = cst2ast(exampleForm);
  println("result:\n<result>");
}


