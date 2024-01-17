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
    | ifElseQuestion: "if (" Expr guard ")" Question ifQuestion "else" Question elseQuestion
    | ifQuestion: "if (" Expr guard ")" Question question
    ;

// TODO: +, -, *, /, &&, ||, !, >, <, <=, >=, ==, !=, literals (bool, int, str)
// Think about disambiguation using priorities and associativity
// and use C/Java style precedence rules (look it up on the internet)
syntax Expr 
    = Id \ "true" \ "false" // true/false are reserved keywords.
    | negation: Negator negator Expr e
    | left computation: Expr a Operator operator Expr b
    | val: Value v
    | parenthesis: "(" Expr a ")"
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

lexical Negator
    = "!"
    > "-"
    ;

start[Form] parseFromFile(loc fileLocation){
    println("HERE!");
    return parse(#start[Form], resolveLocation(fileLocation));
}

start[Form] example() {
    loc fileLocation = |cwd:///examples/errors.myql|;
    return parse(#start[Form], resolveLocation(fileLocation));
}        

start[Form] getForm(int number){
    if(number < 1 || number > 5){
        throw "<number> out of range.\n1: binary.myql\n2: cyclic.myql\n3: empty.myql\n4: errors.myql\n5: tax.myql";
    }
    loc fileLocation;
    switch(number){
        case 1: fileLocation = |cwd:///examples/binary.myql|;
        case 2: fileLocation = |cwd:///examples/cyclic.myql|;
        case 3: fileLocation = |cwd:///examples/empty.myql|;
        case 4: fileLocation = |cwd:///examples/errors.myql|;
        case 5: fileLocation = |cwd:///examples/tax.myql|;
    }
    return parse(#start[Form], resolveLocation(fileLocation));
}

test bool testBinary(){
    try{
        getForm(1);
        println("PASSED: Syntax with binary.myql");
    }
    catch ParseError(location):{
        println("FAILED: Syntax with binary.myql\nlocation: <location>");
        return false;
    }
    return true;
}

test bool testCyclic(){
    try{
        getForm(2);
        println("PASSED: Syntax with cyclic.myql");
    }
    catch ParseError(location): {
        println("FAILED: Syntax with cyclic.myql\nlocation: <location>");
        return false;
    }
    return true;
}

test bool testEmpty(){
    try{
        getForm(3);
        println("PASSED: Syntax with empty.myql");
    }
    catch ParseError(location):{
        println("FAILED: Syntax with empty.myql\nlocation: <location>");
        return false;
    }
    return true;
}

test bool testErrors(){
    try{
        getForm(4);
        println("PASSED: Syntax with errors.myql");
    }
    catch ParseError(location): {
        println("FAILED: Syntax with errors.myql\nlocation: <location>");
        return false;
    }
    return true;
}

test bool testTax(){
     try{
        getForm(5);
        println("PASSED: Syntax with tax.myql");
    }
    catch ParseError(location):{
        println("FAILED: Syntax with tax.myql\nlocation: <location>");
        return false;
    }
    return true;
}



