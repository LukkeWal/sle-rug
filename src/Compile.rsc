module Compile

import CST2AST;
import AST;
import Resolve;
import IO;
import Eval;
import lang::html::AST; // see standard library
import lang::html::IO;

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

void compile(AForm f) {
  HTMLElement t = text("some text");
  println("attribute test: <t>");
  writeFile(f.src[extension="js"].top, form2js(f));
  writeFile(f.src[extension="html"].top, writeHTMLString(form2html(f)));
}

HTMLElement form2html(AForm f) {
  HTMLElement myForm = form([]);
  for (q <- f.questions){
    myForm.elems += question2html(q);
  }
  HTMLElement myHead = head([script([], \src = "https://cdn.jsdelivr.net/npm/vue@2")]);
  HTMLElement myBody = body([myForm]);
  return html([myHead, myBody]);
}

list[HTMLElement] question2html(AQuestion q){
  switch(q){
    case singleQuestion(qLabel, qId, finaltype): {
      HTMLElement lab = label([text("<qLabel>")], \for = "<qId.name>");
      HTMLElement inp = input(\id = "<qId.name>", \type = getInputType(finaltype));
      return [lab, inp, br()];
    }
    case computedQuestion(qLabel, qId, finaltype, expression): { 
      HTMLElement lab = label([text("<qLabel>")], \for = "<qId.name>");
      HTMLElement val = text("value", \id = "<qId.name>");
      return [lab, val, br()];
    }
    case block(questions): return questions2html(questions);
    case ifElseQuestion(conditionId, ifQuestions, elseQuestions): {
      HTMLElement ifConditionTrue = section([text("if: <conditionId.name>"), br()] + questions2html(ifQuestions));
      HTMLElement elseConditionTrue = section([text("else: <conditionId.name>"), br()] + questions2html(elseQuestions));
      return [ifConditionTrue, elseConditionTrue];
    }
    case ifQuestion(conditionId, ifQuestions): {
      HTMLElement ifConditionTrue = section([text("if: <conditionId.name>"), br()] + questions2html(ifQuestions));
      return [ifConditionTrue];
    }
  }
  return [];
}

list[HTMLElement] questions2html(list[AQuestion] qs){
  list[HTMLElement] questionList = [];
  for (q <- qs){
      questionList += question2html(q);
  }
  return questionList;
}

str getInputType(AType t){
  switch (t){
    case stringType(): return "text";
    case booleanType(): return "checkbox";
    default: return "number";
  }
}



str form2js(AForm f) {
  VEnv venv = initialEnv(f);
  return "
  // Import your AST and Eval definitions
  import { AId, AType, AValue } from \'src/AST.rsc\';
  import { initialEnv } from \'src/Eval.rsc\'; // Adjust the path accordingly

  // Call the initialEnv function directly to initialize venv
  const myForm = ...; // Define your form here
  const initializedVenv = initialEnv(myForm);

  // Define a Vue instance
  var app = new Vue({
      el: \'#app\',
      data: {
          questions: [
              // Initialize your questions here with appropriate IDs, labels, and types
              {
                  id: new AId({ name: \'question1\' }),
                  label: \'Question 1\',
                  finaltype: new AType.stringType(),
                  value: \'\' // Initial value
              },
              // Add more questions as needed
          ],
          venv: initializedVenv // Initialize venv directly from Vue
      },
      methods: {
          submitForm: function () {
              // Process the form submission, update VEnv, and perform other actions
              for (let question of this.questions) {
                  // Update VEnv with the input value using the eval function
                  const inputValue = question.finaltype === \'booleanType\'
                      ? question.value // For boolean, use the actual value
                      : new AValue({ value: question.value }); // For other types, wrap it in AValue

                  const input = { question: question.id.name, value: inputValue };
                  this.venv = eval(this.form, input, this.venv);
              }

              // You can now send the updated VEnv to your backend or perform other actions
              console.log(\'Updated VEnv:\', this.venv);
          }
      }
  });
  ";
}

void testCompile(){
  AForm f = getExampleAST();
  println("result: <form2html(f)>");
  compile(f);
  
}
