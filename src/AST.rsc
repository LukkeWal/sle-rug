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
  | ifElseQuestion(AId conditionId, list[AQuestion] ifQuestions, list[AQuestion] elseQuestions)
  | ifQuestion(AId ifId, list[AQuestion] ifQuestions)
  ; 

data AExpr(loc src = |tmp:///|)
  = ref(AId id)
  | computationExpr(AExpr a, str operator, AExpr b)
  | val(AValue v)
  ;


data AId(loc src = |tmp:///|)
  = id(str name);

data AType(loc src = |tmp:///|)
 = stringType()
  | booleanType()
  | integerType()
  ;

data AValue(loc src = |tmp:///|)
 = String()
 | Boolean()
 | Integer()
 ;

 data AOperator
    = mul() 
    | div() 
    | add() 
    | sub()
    | and() 
    | or()
    | neq()
    | leq()
    | geq()
    | eq()
    | greater()
    | less()
    | not()
    ;