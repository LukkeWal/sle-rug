module AST

/*
 * Define Abstract Syntax for QL
 *
 * - complete the following data types
 * - make sure there is an almost one-to-one correspondence with the grammar
 */

data AForm(loc src = |tmp:///|)
  = form(str name, list[AQuestion] questions)
  ; 

data AQuestion(loc src = |tmp:///|)
  = singleQuestion(str label, AId id, AType finaltype)
  | computedQuestion(str label, AId id, AType finaltype, AExpr expression)
  | block(list[AQuestion] questions)
  | ifElseQuestion(AExpr guard, AQuestion ifQuestion, AQuestion elseQuestion)
  | ifQuestion(AExpr guard, AQuestion question)
  ; 

data AExpr(loc src = |tmp:///|)
  = ref(AId id)
  | computation(AExpr a, str operator, AExpr b)
  | negation(str neg, AExpr a)
  | val(AValue v)
  | parenthesis(AExpr a)
  ;


data AId(loc src = |tmp:///|)
  = id(str name);

data AType(loc src = |tmp:///|)
 = stringType()
  | booleanType()
  | integerType()
  ;

data AValue(loc src = |tmp:///|)
 = String(str v)
 | Boolean(bool b)
 | Integer(int i)
 ;