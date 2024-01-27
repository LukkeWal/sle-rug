module Resolve
import IO;
import Syntax;
import CST2AST;
import Transform;

import AST;

/*
 * Name resolution for QL
 */ 


// modeling declaring occurrences of names
alias Def = rel[str name, loc def];

// modeling use occurrences of names
alias Use = rel[loc use, str name];

alias UseDef = rel[loc use, loc def];

// the reference graph
alias RefGraph = tuple[
  Use uses, 
  Def defs, 
  UseDef useDef
]; 

RefGraph resolve(AForm f) = <us, ds, us o ds>
  when Use us := uses(f), Def ds := defs(f);

Use uses(AForm f) {
  Use result = {};
  for (question <- f.questions){
    result += uses(question);
  }
  return result;
}

Use uses(AQuestion q) {
  Use result = {};
  switch(q){
    case computedQuestion(_, _, _, AExpr expression): result += uses(expression);
    case block(questions): {
      for (question <- questions){
        result += uses(question);
      }
    }
    case ifQuestion(AExpr guard, ifQuestion): result += uses(guard) + uses(ifQuestion);
    case ifElseQuestion(AExpr guard, ifQuestion, elseQuestion): result += uses(guard) + uses(ifQuestion) + uses(elseQuestion);
  }
  return result;
}

Use uses(AExpr e) {
  Use result = {};
  switch (e) {
    case ref(x): result += <x.src, "<x.name>">;
    case negation(_, AExpr a): result += uses(a);
    case computation( AExpr a, _, AExpr b): result += uses(a) + uses(b);
    case parenthesis(AExpr a): result += uses(a);
  }
  return result;
}

Def defs(AForm f) {
  Def result = {};
  for (question <- f.questions){
    result += defs(question);
  }
  return result;
}

Def defs(AQuestion q){
  Def result = {};
  switch(q){
    case singleQuestion(_,id,_): result += <"<id.name>", id.src>;
    case computedQuestion(_, AId id, _, _): result += <"<id.name>", id.src>;
    case block(questions): {
      for (question <- questions){
        result += defs(question);
      }
    }
    case ifQuestion(_, AQuestion ifQuestion): result += defs(ifQuestion);
    case ifElseQuestion(_, AQuestion ifQuestion, AQuestion elseQuestion): result += defs(ifQuestion) + defs(elseQuestion);
  }
  return result;
}

bool checkRefGraph(RefGraph r, list[str] correctUse, list[str] correctDef){
  for(use <- r.uses){
    if(!(use.name in correctUse)){
      return false;
    }
  }
  for(def <- r.defs){
    if(!(def.name in correctDef)){
      return false;
    }
  }
  return true;
}

test bool testResolveBinary(){
  println("testing resovle on binary");
  AForm f = flatten(getAST(1));
  RefGraph r = resolve(f);
  return true;
}

test bool testError(){
  AForm f = getAST(4);
  RefGraph r = resolve(f);
  list[str] correctUses = ["hasSodHouse", "sellingPrice", "privateDebt"];
  list[str] correctDefs = ["hasBoughtHouse", "hasMaintLoan", "hasSoldHouse", "sellingPrice", "privateDebt", "valueResidue"];
  return checkRefGraph(r, correctUses, correctDefs);
}

test bool testTax(){
  AForm f = getAST(5);
  RefGraph r = resolve(f);
  list[str] correctUses = ["hasSoldHouse", "sellingPrice", "privateDebt"];
  list[str] correctDefs = ["hasBoughtHouse", "hasMaintLoan", "hasSoldHouse", "sellingPrice", "privateDebt", "valueResidue"];
  return checkRefGraph(r, correctUses, correctDefs);
}