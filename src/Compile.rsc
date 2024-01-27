module Compile

import util::Webserver;
extend Content;
import CST2AST;
import Syntax;
import AST;
import Resolve;
import Transform;
import IO;
import Eval;
import lang::html::AST; // see standard library
import lang::html::IO;
import String;
import Exception;
import Check;
import Message;

loc serverLocation = |http://localhost:10001|;
map[str, AType] idToTypeMap;
VEnv currentVenv;
AForm currentForm;
bool isInitialized = false;

/*
 * Implement a compiler for QL to HTML and Javascript
 *
 * - assume the form is type- and name-correct
 * - separate the compiler in two parts form2html and form2js producing 2 files
 * - use string templates to generate Javascript
 * - use the HTMLElement type and the `str writeHTMLString(HTMLElement x)` function to format to string
 * - use any client web framework (e.g. Vue, React, jQuery, whatever) you like for event handling
 * - map booleans to checkboxes, strings to textfields, ints to numeric text fields
 * - be sure to generate uneditable widgets for computed questions!
 * - if needed, use the name analysis to link uses to definitions
 */

HTMLElement form2html(AForm f) {
  HTMLElement myForm = form([], \method="POST");
  for (q <- f.questions){
    if(!(q is ifQuestion)){
      throw "Expected only if questions in flattened form";
    }
    HTMLElement d;
    switch(q.question){
      case singleQuestion(str qlabel, AId qid, AType qfinaltype): {
        str defaultValue = "";
        if(isInitialized){
          switch(currentVenv[qid.name]){
            case vint(n): defaultValue = "<n>";
            case vbool(b): {
              switch(b){
                case true: defaultValue = "on";
                case false: defaultValue = "off";
              }
            }
            case vstr(s): defaultvalue = s;
          }
        }
        HTMLElement lab = label([text("<qlabel>")]);
        HTMLElement inp;
        switch(qfinaltype){
          case booleanType(): {
            if(defaultValue == "on"){
              inp = input(\type = getInputType(qfinaltype), \name = "<qid.name>", \checked = "");
            } else {
              inp = input(\type = getInputType(qfinaltype), \name = "<qid.name>");
            }
          }
          default: inp = input(\type = getInputType(qfinaltype), \name = "<qid.name>", \value = defaultValue);
        }
        d = div([lab, inp], \id = "<qid.name>");
      }
      case computedQuestion(str qlabel, AId qid, AType qfinaltype, AExpr qexpression):{
        HTMLElement lab = label([text("<qlabel>")], \for = "<qid.name>");
        HTMLElement val;
        switch(qfinaltype){
          case stringType(): val = text("<currentVenv[qid.name].s>");
          case booleanType(): val = text("<currentVenv[qid.name].b>");
          case integerType(): val = text("<currentVenv[qid.name].n>");
        }
        d = div([lab, val], \id = "<qid.name>");
      }
      default: throw "Expected if questions with only single or computed questions in them";
    }
    // Check if this question should be displayed
    if(q.guard == val(Boolean(true))){ // This question is not in an if statement
      myForm.elems += [d, br()];
    } else if (isInitialized && eval(q.guard, currentVenv) == vbool(true)){
      myForm.elems += [d, br()];
    }
  }
  myForm.elems += [input(\type="submit", \value="Submit")];
  HTMLElement myBody = body([myForm]);
  return html([myBody]);
}

str getInputType(AType t){
  switch (t){
    case stringType(): return "text";
    case booleanType(): return "checkbox";
    default: return "number";
  }
}

map[str, AType] createTypeMap(AForm f){
  map[str, AType] result = ();
  for(q <- f.questions){
    result += (q.question.id.name:q.question.finaltype);
  }
  return result;
}

void startWebServer() {
   // simple get
   Response testServer(r:get("/")) = getResponse(true, r);
   Response testServer(p:post("/", value (type[value] _) stuff)) = getResponse(false, p);     
   try {
      serve(serverLocation, testServer);
      println("Server started successfully!\n<serverLocation>");
   }
   catch value exception:
     throw "failed to server website: <exception>";
}

Response getResponse(bool isGet, Request r){
  loc inputHTMLLocation = |cwd:///examples/input_form.html|;
  if(isGet){
    // respond with give ql input
    return response(resolveLocation(inputHTMLLocation));
  }
  if("QLInput" in r.parameters){
    // respond with their compiled ql
    try
      currentForm = flatten(cst2ast(parseFromString(r.parameters["QLInput"])));
    catch e:
      return response("There was an error while handling your input:\n<e>");
    set[Message] errorsOrWarnings = check(currentForm);
    str totalErrorsOrWarnings = "";
    for(msg <- errorsOrWarnings){
      switch(msg){
        case error(str errorMessage): totalErrorsOrWarnings = "<totalErrorsOrWarnings>\<br\>ERROR:<errorMessage>";
        case warning(str warningMessage): totalErrorsOrWarnings = "<totalErrorsOrWarnings>\<br\>WARNING:<warningMessage>";
      }
    }
    if (totalErrorsOrWarnings != ""){
      return response(totalErrorsOrWarnings);
    }
    currentVenv = initialEnv(currentForm);
    idToTypeMap = createTypeMap(currentForm);
    isInitialized = true;
    return response(writeHTMLString(form2html(currentForm)));
  }
  // respond with their compiled and re-evaluated ql
  for(str key <- r.parameters){
    if(key in currentVenv){
      Value v;
      switch(idToTypeMap[key]){
        case stringType(): v = vstr(r.parameters[key]);
        case booleanType(): {
          switch("<r.parameters[key]>"){
            case "on": v = vbool(true);
            case "off": v = vbool(false);
            default: throw "booleanType result undefined: <r.parameters[key]>";
          }
        }
        case integerType(): v = vint(toInt(r.parameters[key]));
        default: throw "no type for <key> with type <idToTypeMap[key]>";
      }
      println("new input: <key> <v>");
      currentVenv = eval(currentForm, input(key, v), currentVenv);
    }
  }
  for(str key <- currentVenv){
    switch(currentVenv[key]){
      case vbool(b): { // when checkboxes are not included in a form submission, it means they are unchecked
        if (!(key in r.parameters)){
          println("new input: <key> <vbool(false)>");
          currentVenv = eval(currentForm, input(key, vbool(false)), currentVenv);
        }
      }
    }
  }
  return response(writeHTMLString(form2html(currentForm)));
}

void shutDownServer(){
  shutdown(serverLocation);
}
