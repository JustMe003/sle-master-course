module App

import salix::HTML;
import salix::App;
import salix::Core;
import salix::Index;

import Eval;
import Syntax;
import IO;

import String;

import lang::std::Id;


// The salix application model is a tuple
// containing the questionnaire and its current run-time state (env).
alias Model = tuple[start[Form] form, VEnv env];

App[Model] runQL(start[Form] ql) = webApp(qlApp(ql), |project://sle-master-course/src|);

SalixApp[Model] qlApp(start[Form] ql, str id="root") 
  = makeApp(id, 
        Model() { return <ql, initialEnv(ql)>; }, 
        withIndex("<ql.top.title>"[1..-1], id, view, css=["https://cdn.simplecss.org/simple.min.css"]), 
        update);


// The salix Msg type defines the application events.
data Msg
  = updateInt(str name, str n)
  | updateBool(str name, bool b)
  | updateStr(str name, str s)
  ;

// We map messages to Input values 
// to be able to reuse the interpreter defined in Eval.
Input msg2input(updateInt(str q, str n)) = user(q, vint(toInt(n)));
Input msg2input(updateBool(str q, bool b)) = user(q, vbool(b));
Input msg2input(updateStr(str q, str s)) = user(q, vstr(s));

// The Salix model update function simply evaluates the user input
// to obtain the new state. 
Model update(Msg msg, Model model) = model[env=eval(model.form, msg2input(msg), model.env)];

// Salix view rendering works by "drawing" on an implicit HTML canvas.
// Look at the Salix demo folder to learn how html elements are drawn, and how element nesting is achieved with
// nesting of void-closures.
void view(Model model) {
  h3("<model.form.top.title>"[1..-1]);
  list[Question] questions = render(model.form.top, model.env);
  for (Question q <- questions) {
    viewQuestion(q, model);
  }
}

// fill in: question rendering, but only if they are enabled.
void viewQuestion(Question q, Model model) {
  switch(q) {
    case answerable(Str prompt, Id name, Type t): {
      p("<prompt>"[1..-1]);
      viewInput(name, t, model.env["<name>"]);
    }
    case computed(Str prompt, Id name, Type t, Expr e): {
      p("<prompt>"[1..-1]);
    }
  }
}

void viewInput(Id name, Type t, Value v) {
  Msg strHandler(str s) = updateStr("<name>", s);
  Msg intHandler(str i) = updateInt("<name>", i);
  int i = 0;
  bool b = false;
  str s = "";
  switch(v) {
    case vbool(b2): {b = b2; println(b2);}
    case vint(i2): i = i2;
    case vstr(s2): s = s2;
  }
  Msg boolHandler(bool _) = updateBool("<name>", !b);
  switch("<t>") {
    case "string": input([id("<name>_input"), \type("text"), onChange(strHandler), \value("<s>")]);
    case "integer": input([id("<name>_input"), \type("number"), \value("<i>"), onChange(intHandler)]);
    case "boolean": input([id("<name>_input"), \type("checkbox"), checked(b), onCheck(boolHandler)]);
  }
}