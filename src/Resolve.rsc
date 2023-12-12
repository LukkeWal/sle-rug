module Resolve
import IO;
import Syntax;
import CST2AST;

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
    result = result + questionUses(question);
  }
  return result;
}

Use questionUses(AQuestion q) {
  Use result = {};
  switch(q){
    case computedQuestion(_, _, _, expression):
      result = result + expressionUses(expression);
    case block(questions): {
      for (question <- questions) {
        result = result + questionUses(question);
      }
    }
    case ifQuestion(conditionId, ifQuestions):{
      result = result + <conditionId.src, "<conditionId.name>">;
      for (question <- ifQuestions) {
        result = result + questionUses(question);
      }
    }
    case ifElseQuestion(conditionId, ifQuestions, elseQuestions):{
      result = result + <conditionId.src, "<conditionId.name>">;
      for (question <- ifQuestions) {
        result = result + questionUses(question);
      }
      for (question <- elseQuestions) {
        result = result + questionUses(question);
      }
    }
  }
  return result;
}

Use expressionUses(AExpr e) {
  Use result = {};
  switch (e) {
    case ref(x):
      result = result + <x.src, "<x.name>">;
    case computationExpr(a, _, b): {
      result = result + expressionUses(a);
      result = result + expressionUses(b);
    }
  }
  return result;
}

Def defs(AForm f) {
  Def result = {};
  for (question <- f.questions){
    result = result + questionDefs(question);
  }
  return result;
}

Def questionDefs(AQuestion q){
  Def result = {};
  switch(q){
    case singleQuestion(_,id,_):
      result = result + <"<id.name>", id.src>;
    case computedQuestion(_, id, _, _):
      result = result + <"<id.name>", id.src>;
    case block(questions): {
      for (question <- questions) {
        result = result + questionDefs(question);
      }
    }
    case ifQuestion(_, ifQuestions):{
      for (question <- ifQuestions) {
        result = result + questionDefs(question);
      }
    }
    case ifElseQuestion(_, ifQuestions, elseQuestions): {
      for (question <- ifQuestions) {
        result = result + questionDefs(question);
      }
      for (question <- elseQuestions) {
        result = result + questionDefs(question);
      }
    }
  }
  return result;
}

void testResolve(){
  start[Form] exampleForm = example();
  AForm AexampleForm = cst2ast(exampleForm);
  println("from:\n<exampleForm>");
  println("Afrom:\n<AexampleForm>");
  println("uses:\n<uses(AexampleForm)>");
  println("defs:\n<defs(AexampleForm)>");
  println("refGraph:\n<resolve(AexampleForm)>");
}