module Check

import AST;
import Resolve;
import Message; // see standard library
import Transform;

import CST2AST;
import IO;
import Set;

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
  return result;
}

Type ATypeToType(AType t){
  switch(t){
    case stringType(): return tstr();
    case booleanType(): return tbool();
    case integerType(): return tint();
    default: return tunknown();
  }
}

set[Message] check(AForm f){
  RefGraph refGraph = resolve(f);
  TEnv tenv = collect(f);
  set[Message] result = check(f, tenv, refGraph.useDef);
  return result;
}

// - produce an error if there are used names that do not have a definition
set[Message] check(AForm f, TEnv tenv, UseDef useDef) {
  set[Message] result = {};
  result += checkForCycles(f);
  for(question <- f.questions){
    result += check(question, tenv, useDef);
  }
  return result; 
}

// - produce an error if there are declared questions with the same name but different types.
// - duplicate labels should trigger a warning 
// - the declared type computed questions should match the type of the expression.
set[Message] check(AQuestion q, TEnv tenv, UseDef useDef) {
  set[Message] result = {};
  for (tuple[loc def, str name, str label, Type t] x <- tenv){
    if (q is computedQuestion || q is singleQuestion){
      // - produce an error if there are declared questions with the same name but different types.
      if (q.id.name == x.name && ATypeToType(q.finaltype) != x.t){
          result += error("<x.name> has multiple definitions with different types.", x.def);
        };
      // - duplicate labels should trigger a warning 
      if (q.label == x.label && q.id.name != x.name){
          result += warning("<x.name> and <q.id.name> have the same label.", x.def);
        };
      switch(q){
        case computedQuestion(_,id,finaltype,_): {
          // - the declared type computed questions should match the type of the expression.
          if (x.def in useDef.use && ATypeToType(finaltype) != x.t){ // this tenv question is used by q
            result += error("<x.name> is used by <id.name> but does not have the same type", x.def);
          };
        }
      }
    }
  };
  // Check any other questions or expressions inside this question
  switch(q){
    case computedQuestion(_, _, _, AExpr expression): result += check(expression, tenv, useDef);
    case block(list[AQuestion] questions): {
      for(question <- questions){
        result += check(question, tenv, useDef);
      }
    }
    case ifElseQuestion(AExpr guard, AQuestion ifQuestion, AQuestion elseQuestion): result += check(guard, tenv, useDef) + check(ifQuestion, tenv, useDef) + check(elseQuestion, tenv, useDef);
    case ifQuestion(AExpr guard, AQuestion question): {
      result += check(guard, tenv, useDef);
      result += check(question, tenv, useDef);
    }
  }
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
    case negation(str neg, AExpr a): {
      Type expectedType;
      switch(neg){
        case "!": expectedType = tbool();
        case "-": expectedType = tint();
      }
      if(typeOf(a, tenv,useDef) != expectedType){
        msgs += { error("Invalid operand type: expected <expectedType> but got <typeOf(a, tenv, useDef)>", a.src)};
      }
    }
    case computation(AExpr a, str operator, AExpr b): {
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
        default: throw "unimplemented operator: <operator>";
      }
      if (typeOf(a, tenv, useDef) != typeOf(b, tenv, useDef)){
          msgs += { error("Invalid operand types: LHS and RHS are not the same.", a.src)};
      } else if (!anyType && typeOf(a, tenv, useDef) != expectedType){
        msgs += { error("Invalid operand types: expected <expectedType> but got <typeOf(a, tenv, useDef)>", a.src)};
      }
    }
  }
  // check any expressions within the given expression
  switch(e){
    case negation(_, AExpr a): msgs += check(a, tenv, useDef);
    case computation(AExpr a, _, AExpr b): msgs += check(a, tenv, useDef) + check(b, tenv, useDef);
    case parenthesis(AExpr a): msgs += check(a, tenv, useDef);
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

// modeling users and their uses of names
alias User = rel[str user, str uses];

User closeUserRelation(AForm f) = usr o uss
  when User usr := getUsers(f), User uss := getUsers(f);

User getUsers(AForm f){
  f = flatten(f);
  User result = {};
  for(q <- f.questions){
    AExpr guard = q.guard;
    switch(q.question){
      case singleQuestion(str label, AId id, AType finaltype): {
        for(use <- getUsesFromAExpr(guard)){
          result += <id.name, use>;
        }
      }
      case computedQuestion(str label, AId id, AType finaltype, AExpr expression): {
        for(use <- getUsesFromAExpr(guard)){
          result += <id.name, use>;
        }
        for(use <- getUsesFromAExpr(expression)){
          result += <id.name, use>;
        }
      }
    }
  }
  return result;
}

list[str] getUsesFromAExpr(AExpr a){
  list[str] result = [];
  switch(a){
    case ref(AId id): result += id.name;
    case negation(str neg, AExpr a): result += getUsesFromAExpr(a);
    case computation(AExpr a, str operator, AExpr b): result += getUsesFromAExpr(a) + getUsesFromAExpr(b);
    case val(AValue v): ;
    case parenthesis(AExpr a): result += getUsesFromAExpr(a);
  }
  return result;
}

set[Message] checkForCycles(AForm f){
  set[Message] result = {};
  User user = closeUserRelation(f);
  for (u <- user){
    if (u.user == u.uses){
      result += {error("Cyclic definition: <u.user>")};
    }
  }
  return result;
}

test bool testCheckBinary(){
  AForm f = getAST(1);
  TEnv tenv = collect(f);
  UseDef useDef = resolve(f).useDef;
  set[Message] result = check(f, tenv, useDef);
  for(msg <- result){
    switch(msg){
      case error(errorMsg): {
        return false;
      }
    }
  }
  return true;
}

test bool testCyclic(){
  AForm f = getAST(2);
  UseDef useDef = resolve(f).useDef;
  set[Message] messages = checkForCycles(f);
  if(size(messages)== 4){
    return true;
  }
  return false;
}

 test bool testError(){
  AForm f = getAST(4);
  TEnv tenv = collect(f);
  UseDef useDef = resolve(f).useDef;
  set[Message] result = check(f, tenv, useDef);
  if (size(result) == 3){
    return true;
  }
  return false;
 }

 test bool testTax(){
  AForm f = getAST(5);
  TEnv tenv = collect(f);
  UseDef useDef = resolve(f).useDef;
  set[Message] result = check(f, tenv, useDef);
  if (size(result) == 0){
    return true;
  }
  return false;
 }
 
 

