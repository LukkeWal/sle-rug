module CST2AST
import IO;
import Syntax;
import AST;

import ParseTree;
import String;
import Boolean;

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
    case ifQuestion(guard, question):
      return ifQuestion(cst2ast(guard), cst2ast(question), src=q.src);
    case ifElseQuestion(Expr guard, Question ifQuestion, Question elseQuestion): 
      return ifElseQuestion(cst2ast(guard), cst2ast(ifQuestion), cst2ast(elseQuestion), src=q.src);
    default: throw "Unhandled question: <q>";
  }
}

AExpr cst2ast(Expr e) {
  switch (e) {
    case (Expr)`<Id x>`: return ref(id("<x>", src=x.src), src=x.src);
    case computation(Expr a, Operator operator, Expr b): return computation(cst2ast(a), "<operator>", cst2ast(b), src=e.src);
    case negation(Negator negator, Expr e): return negation("<negator>", cst2ast(e), src=e.src);
    case val(Value v): return val(cst2ast(v), src=e.src);
    case parenthesis(Expr a): return parenthesis(cst2ast(a), src=e.src);
    default: throw "Unhandled expression: <e>";
  }
}

AValue cst2ast(Value v){
  switch(v){
    case string(Str s): return String("<s>", src=v.src);
    case integer(Int i): return Integer(toInt("<i>"), src=v.src);
    case boolean(Bool b): return Boolean(fromString("<b>"), src=v.src);
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

test bool testBinary(){
    try{
        cst2ast(getForm(1));
        println("PASSED: CST2AST with binary.myql");
    }
    catch ParseError(location):{
        println("FAILED: CST2AST with binary.myql\nlocation: <location>");
        return false;
    }
    return true;
}

test bool testCyclic(){
    try{
        cst2ast(getForm(2));
        println("PASSED: CST2AST with cyclic.myql");
    }
    catch ParseError(location): {
        println("FAILED: CST2AST with cyclic.myql\nlocation: <location>");
        return false;
    }
    return true;
}

test bool testEmpty(){
    try{
        cst2ast(getForm(3));
        println("PASSED: CST2AST with empty.myql");
    }
    catch ParseError(location):{
        println("FAILED: CST2AST with empty.myql\nlocation: <location>");
        return false;
    }
    return true;
}

test bool testErrors(){
    try{
        cst2ast(getForm(4));
        println("PASSED: CST2AST with errors.myql");
    }
    catch ParseError(location): {
        println("FAILED: CST2AST with errors.myql\nlocation: <location>");
        return false;
    }
    return true;
}

test bool testTax(){
     try{
        cst2ast(getForm(5));
        println("PASSED: CST2AST with tax.myql");
    }
    catch ParseError(location):{
        println("FAILED: CST2AST with tax.myql\nlocation: <location>");
        return false;
    }
    return true;
}