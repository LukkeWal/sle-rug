module Check

import AST;
import Resolve;
import Message; // see standard library

import CST2AST;
import IO;

data Type
  = tint()
  | tbool()
  | tstr()
  | tunknown()
  ;

// the type environment consisting of defined questions in the form 
alias TEnv = rel[loc def, str name, str label, Type \type];

// To avoid recursively traversing the form, use the `visit` construct
// or deep match (e.g., `for (/question(...) := f) {...}` ) 
TEnv collect(AForm f) {
  TEnv result = {};
  visit(f){
    case singleQuestion(label,id,finaltype): result = result + {<id.src, id.name, label, ATypeToType(finaltype)>};
    case computedQuestion(label,id,finaltype,_): result = result + {<id.src, id.name, label, ATypeToType(finaltype)>};
  }
  println("TEnv RESULT:\n<result>");
  return result;
}

Type ATypeToType(AType t){
  switch(t){
    case stringType: return tstr();
    case booleanType: return tbool();
    case integerType: return tint();
    default: return tunknown();
  }
}

set[Message] check(AForm f, TEnv tenv, UseDef useDef) {
  return {}; 
}

// - produce an error if there are declared questions with the same name but different types.
// - duplicate labels should trigger a warning 
// - the declared type computed questions should match the type of the expression.
set[Message] check(AQuestion q, TEnv tenv, UseDef useDef) {
  set[Message] result = {};
  for (tuple[loc def, str name, str label, Type t] x <- tenv){
    // - produce an error if there are declared questions with the same name but different types.
    if (q.id.name == x.name && ATypeToType(q.finaltype) != x.t){
        result += error("<x.name> has multiple definitions with different types.", x.def);
      };
    // - duplicate labels should trigger a warning 
    if (q.label == x.label){
        result += warning("<x.name> and <q.id.name> have the same label.", x.def);
      };
    switch(q){
      case computedQuestion(label,id,finaltype,expression): {
        // - the declared type computed questions should match the type of the expression.
        if (x.def in useDef.use && ATypeToType(q.finaltype) != x.t){ // this tenv question is used by q
          result += error("<x.name> is used by <q.id.name> but does not have the same type", x.def);
        };
      }
    }
  };
  return result; 
}

// Check operand compatibility with operators.
// E.g. for an addition node add(lhs, rhs), 
//   the requirement is that typeOf(lhs) == typeOf(rhs) == tint()
set[Message] check(AExpr e, TEnv tenv, UseDef useDef) {
  set[Message] msgs = {};
  
  switch (e) {
    case ref(AId x): {
      msgs += { error("Undeclared question", x.src) | useDef[x.src] == {} };
    }
    case computationExpr(AExpr a, str operator, AExpr b): {
        Type expectedType;
        bool anyType = false;
        switch (operator){
          case "*": expectedType = tint();
          case "/": expectedType = tint();
          case "+": expectedType = tint();
          case "-": expectedType = tint();
          case "&&": expectedType = tbool();
          case "||": expectedType = tbool();
          case "!=": anyType = true;
          case "\<=": expectedType = tint();
          case "\>=": expectedType = tint();
          case "==": anyType = true;
          case "\>": expectedType = tint();
          case "\<": expectedType = tint();
          case "!": expectedType = tbool();
        }
      if (typeOf(a, tenv, useDef) != typeOf(b, tenv, useDef)){
          msgs += { error("Invalid operand types: LHS and RHS are not the same.", a.src)};
      } else if (!anyType && typeOf(a, tenv, useDef) != expectedType){
        msgs += { error("Invalid operand types: expected <expectedType> but got <typeOf(a, tenv, useDef)>", a.src)};
      }
    }
  }
  
  return msgs; 
}

Type typeOf(AExpr e, TEnv tenv, UseDef useDef) {
  switch (e) {
    case ref(id(_, src = loc u)):  
      if (<u, loc d> <- useDef, <d, x, _, Type t> <- tenv) {
        return t;
      }
    case computationExpr(AExpr a, str operator, AExpr b): {
      Type typeA = typeOf(a, tenv, useDef);
      Type typeB = typeOf(b, tenv, useDef);
      if (typeA == typeB){
        return typeA;
      } else {
        return tunknown();
      }
    }
    case val(v): return tint();
  }
  return tunknown(); 
}

/* 
 * Pattern-based dispatch style:
 * 
 * Type typeOf(ref(id(_, src = loc u)), TEnv tenv, UseDef useDef) = t
 *   when <u, loc d> <- useDef, <d, x, _, Type t> <- tenv
 *
 * ... etc.
 * 
 * default Type typeOf(AExpr _, TEnv _, UseDef _) = tunknown();
 *
 */

 void testCheck(){
  AForm a = getExampleAST();
  println("starting with form:\n<a>");
  collect(a);
 }
 
 

