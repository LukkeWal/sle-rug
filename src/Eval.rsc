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
    case singleQuestion(label, id, finaltype): {
      // Update the environment with the input value
      switch(inp){
        case input(q, v): {
          if (q.id.name == id.name) {
            venv[id.name] = v;
          };
        }
      }
    }
    case computedQuestion(label, id, finaltype, expression): venv[id.name] = eval(expression, venv);
  }
  return (); 
}

Value eval(AExpr e, VEnv venv) {
  switch (e) {
    case ref(id(str x)): return venv[x];
    case computationExpr(AExpr a, str operator, AExpr b): {
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
        case "\>": return vbool(eval(a, venv).b > eval(b, venv).b);
        case "\<": return vbool(eval(a, venv).b < eval(b, venv).b);
        default: return vbool(eval(a, venv).b != eval(b, venv).b); // not sure how ! is supposed to work so made it !=
      }
    }
    case val(AValue v): 
    switch(v) {
      case String(): return vstr("");
      case Boolean(): return vbool(false);
      default: return vint(0);
    }
    default: throw "Unsupported expression <e>";
  }
}