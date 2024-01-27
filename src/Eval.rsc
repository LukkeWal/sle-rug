module Eval

import AST;
import Resolve;
import CST2AST;
import IO;

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
  map[str name, Value \value] result = ();
  visit(f){
    case singleQuestion(label,id,finaltype): result[id.name] = ATypeToValue(finaltype);
    case computedQuestion(label,id,finaltype,expression): result[id.name] = ATypeToValue(finaltype);
  }
  return result;
}

Value ATypeToValue(AType t){
  switch(t){
    case stringType(): return vstr("");
    case booleanType(): return vbool(false);
    default: return vint(0);
  }
}


// Because of out-of-order use and declaration of questions
// we use the solve primitive in Rascal to find the fixpoint of venv.
VEnv eval(AForm f, Input inp, VEnv venv) {
  return solve (venv) {
    venv = evalOnce(f, inp, venv);
  }
}

VEnv evalOnce(AForm f, Input inp, VEnv venv) {
  for (AQuestion q <- f.questions) {
    venv = eval(q, inp, venv);
  }
  return venv;
}

VEnv eval(AQuestion q, Input inp, VEnv venv) {
  // evaluate conditions for branching,
  // evaluate inp and computed questions to return updated VEnv
  switch(q){
    case singleQuestion(_, id, _): {
      // Update the environment with the input value
      if(inp.question == id.name){
        venv[id.name] = inp.\value;
      }
    }
    case computedQuestion(_, id, _, expression): venv[id.name] = eval(expression, venv);
    case block(questions):{
      for(question <- questions){
        venv = eval(question, inp, venv);
      }
    }
    case ifElseQuestion(guard, ifQuestion, elseQuestion): {
      venv = eval(ifQuestion, inp, venv);
      venv = eval(elseQuestion, inp, venv);
    }
    case ifQuestion(guard, ifQuestion): venv = eval(ifQuestion, inp, venv);
  }
  return venv; 
}

Value eval(AExpr e, VEnv venv) {
  switch (e) {
    case ref(id(str x)): return venv[x];
    case negation(str negator, AExpr a): {
      switch(negator){
        case "!": return vbool(!eval(a, venv).b);
        case "-": return vint(-1 * eval(a, venv).n);
        default: throw "unknown negator: <negator>";
      }
    }
    case computation(AExpr a, str operator, AExpr b): {
      switch (operator){
        case "*": return vint(eval(a, venv).n * eval(b, venv).n);
        case "/": return vint(eval(a, venv).n / eval(b, venv).n);
        case "+": return vint(eval(a, venv).n + eval(b, venv).n);
        case "-": return vint(eval(a, venv).n - eval(b, venv).n);
        case "&&": return vbool(eval(a, venv).b && eval(b, venv).b);
        case "||": return vbool(eval(a, venv).b || eval(b, venv).b);
        case "!=": return vbool(eval(a, venv) != eval(b, venv));
        case "\<=": return vbool(eval(a, venv).b <= eval(b, venv).b);
        case "\>=": return vbool(eval(a, venv).b >= eval(b, venv).b);
        case "==": return vbool(eval(a, venv) == eval(b, venv));
        case "\>": return vbool(eval(a, venv).n > eval(b, venv).n);
        case "\<": return vbool(eval(a, venv).n < eval(b, venv).n);
        default: throw "unknown operator: <operator>";
      }
    }
    case val(AValue v): 
    switch(v) {
      case String(s): return vstr(s);
      case Boolean(b): return vbool(b);
      case Integer(i): return vint(i);
      default: throw "unimplemented AValue: <v>";
    }
    case parenthesis(AExpr a): return eval(a, venv);
    default: throw "Unsupported expression <e>";
  }
}

test bool testTax(){
  AForm f = getAST(5);
  VEnv venv = initialEnv(f);
  list[Input] allInput = [input("hasBoughtHouse", vbool(true)),
                          input("hasMaintLoan", vbool(true)),
                          input("hasSoldHouse", vbool(true)),
                          input("sellingPrice", vint(1000)),
                          input("privateDebt", vint(250))];
  for(input <- allInput){
    venv = eval(f, input, venv);
  }
  if(venv["valueResidue"] == vint(750)){
    return true;
  }
  return false;
}

test bool testBinary(){
  AForm f = getAST(1);
  VEnv venv = initialEnv(f);
  list[Input] allInput = [input("x_1_10", vbool(true)),
                          input("x_1_5", vbool(true)),
                          input("x_1_3", vbool(true)),
                          input("x_1_2", vbool(true))];
  for(input <- allInput){
    venv = eval(f, input, venv);
  }
  if(venv["answer_1_2"] == vint(1)){
    return true;
  }
  return false;
}