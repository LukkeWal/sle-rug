module Syntax

import IO;
import ParseTree;

extend lang::std::Layout;
extend lang::std::Id;

/*
 * Concrete syntax of QL
 */

start syntax Form 
    = "form" Id name "{" Question* questions "}"; 

// TODO: question, computed question, block, if-then-else, if-then
syntax Question 
    = singleQuestion: Str label Id id ":" Type finaltype
    | computedQuestion: Str label Id id ":" Type finaltype "=" Expr expression
    | block: "{"Question* questions"}"
    | ifElseQuestion: "if (" Id conditionId ")" Question+ ifQuestions "else" Question+ elseQuestions
    | ifQuestion: "if (" Id ifId ")" Question+ ifQuestions
    ;

// TODO: +, -, *, /, &&, ||, !, >, <, <=, >=, ==, !=, literals (bool, int, str)
// Think about disambiguation using priorities and associativity
// and use C/Java style precedence rules (look it up on the internet)
syntax Expr 
    = Id \ "true" \ "false" // true/false are reserved keywords.
    | left Computation: Expr a Operator operator Expr b
    | val: Value v
    ;

syntax Type 
    = String: "string"
    | Boolean: "boolean"
    | Integer: "integer"
    ;

syntax Value
    = string: Str
    | integer: Int
    | boolean: Bool
    ;

lexical Str 
 //   = [a-zA-Z0-9]+;
    = [\"] ![\"]* [\"]; // slightly simplified

lexical Int 
    = [0-9]+;

lexical Bool 
    = "true" 
    | "false"
    ;

lexical Operator
    = "*" 
    > "/" 
    > "+" 
    > "-"
    > "&&" 
    > "||"
    > "!="
    > "\<="
    > "\>="
    > "=="
    > "\>"
    > "\<"
    > "!"
    ;

map[str, value] syntax2map(start[Form] form) = syntax2map(form.top);
map[str, value] syntax2map(Form form) = ("<form.name>": syntax2map(form.questions));
map[str, value] syntax2map(Question* questions){
    map[str, value] result = ();
    for (question <- questions){
        switch(question){
            case singleQuestion(_, _, _): result["singleQuestion <question.id> returns <question.finaltype>"] = "<question.label>";
            case computedQuestion(_, _, _, _): result["computedQuestion <question.id>  returns <question.finaltype>"] = "<question.label> = <question.expression>";
            case block(_): result["block"] = syntax2map(question.questions);
            case ifQuestion(_, _): result["ifQuestion <question.ifId>"] = syntax2map(question.ifQuestions);
            case ifElseQuestion(_, _, _): result["ifElse <question.conditionId>"] = syntax2map(question.elseQuestions);
            default: println("undefined question type");
        }
    }
    return result;
}
default map[str, value] syntax2map(value v){
    println("found unimplemented value");
    return ();
}

start[Form] example() {
    loc fileLocation = |cwd:///examples/tax.myql|;
    return parse(#start[Form], resolveLocation(fileLocation));
}        


void testSyntax() {
    value form = example();
    start[Form] f = example();
    println("source: <f.src>");
    println("Successfully parsed:\n<form>");
    println("Map of form:\n<syntax2map(form)>");
}



