module Compile

import Syntax;
import Eval;
import IO;
import ParseTree;
import String;


import lang::html::AST; // modeling HTML docs
import lang::html::IO; // reading/writing HTML
import lang::std::Id;

int counter = 0;
str valueChangedFunc = "inputChanged";


void compile(start[Form] form) {
  loc h = form.src[extension="html"];
  loc j = form.src[extension="js"].top;
  str js = compile2js(form);
  HTMLElement ht = compile2html(form);
  writeFile(j, js);
  writeHTMLFile(h, ht, escapeMode=extendedMode());
}

str getIfId(loc src) = "if-<stripLoc(src)>";
str getElseId(loc src) = "else-<stripLoc(src)>";
str getInputId(loc src) = "input-<stripLoc(src)>";
str getCompId(loc src) = "comp-<stripLoc(src)>";
str getRepeatId(loc src) = "repeat-<stripLoc(src)>";

str stripLoc(loc l) {
  int last = findLast("<l>", "|");
  return "<l>"[last + 2..-1];
}

str stripFileLoc(loc l) {
  int last = findLast("<l>", "/");
  return "<l>"[last + 1..-1];
}

value value2usable(vbool(b)) = b;
value value2usable(vint(n)) = n;
value value2usable(vstr(s)) = "\"<s>\"";
value value2usable(vlist(l)) = l;

str compile2js(start[Form] form) {
  map[str, list[str]] depends = ();
  map[str, Eval::Value] values = ();
  set[str] repeats = {};

  void addExpressionDependency(str name, Expr e) {
    set[str] l = {};
    visit(e) {
      case var(Id n): l += {"<n>"};
      case index(Id n, Expr _): l += {"<n>"};
    }
    for (id <- l) {
      if (id notin depends) depends[id] = [];
      depends[id] += [name];
    }
  }
  list[str] getQuestionDependencies(Question q) {
    switch(q) {
      case computed(Str _, Id name, Type _, Expr _): return ["<getSymbolNameComputed(name.src)>"];
      case block(Question *qs): return ( [] | it + getQuestionDependencies(q1) | q1 <- qs);
      default: return [];
    }
  }
  void addQuestionDependency(Question q, str dep) {
    list[str] l = getQuestionDependencies(q);
    if (size(l) > 0) {
      if (dep notin depends) depends[dep] = [];
      for (str id <- getQuestionDependencies(q)) {
        depends[dep] += id;
      }
    }
  }
  void markRepeats(Question q) {
    visit(q) {
      case answerable(Str _, Id name, Type _): repeats += {"<name>"};
    }
  }

  visit(form.top.questions) {
    case computed(Str _, Id name, Type t, Expr e): { addExpressionDependency(getSymbolNameComputed(name.src), e); values["<name>"] = type2default(t); }
    case ifThen(Expr cond, Question q): { addExpressionDependency(getIfId(cond.src), cond); addQuestionDependency(q, getIfId(cond.src)); }
    case elseThen(Expr cond, Question ifThen, Question elseThenQ): { addExpressionDependency(getIfId(cond.src), cond); addQuestionDependency(ifThen, getIfId(cond.src)); addQuestionDependency(elseThenQ, getElseId(cond.src)); }
    case answerable(Str _, Id name, Type t): { values["<name>"] = type2default(t); }
    case repeat(Expr e, Question q): { addExpressionDependency(getSymbolNameRepeat(e.src), e); markRepeats(q); }
  }

  return 
  "<for (str key <- values) {> let <key>_value = <key in repeats ? [] : value2usable(values[key])>;
   '<}>
   '
   '
   '// answerable questions
   '<for (/answerable(Str _, Id var, Type t) := form.top) {>
   'function <getSymbolNameAnswerable(var.src)>(n) {
   '  <getAnswerableBody(depends, var, t, "<var>" in repeats)>
   '}
   '<}>
   '
   '// computable questions
   '<for (/computed(Str _, Id var, Type t, Expr e) := form.top) {>
   'function <getSymbolNameComputed(var.src)>() {
   '  <getComputedBody(depends, var, t, e)>
   '}
   '<}>
   '
   '// If statements
   '<for (/ifThen(Expr cond, Question _) := form.top) {>
   'function <getSymbolNameIf(cond.src)>() {
   '  <getIfBody(depends, cond)> 
   '}
   '<}>
   '
   '// If else statements
   '<for (/elseThen(Expr cond, Question _, Question _) := form.top) {>
   'function <getSymbolNameIf(cond.src)>() {
   '  <getIfElseBody(depends, cond)> 
   '}
   '<}>
   '
   '// Repeat statements
   '<for (/repeat(Expr e, Question _) := form.top) {>
   'function <getSymbolNameRepeat(e.src)>() {
   '  <getRepeatBody(depends, e)>
   '}
   '<}>
   '
   '// Calling each function at least once
   'function init() {
   '  <for (Question q <- form.top.questions) {>
   '<for (str s <- getFunctionCall(q)) {>  <s>\n<}>
   '  <}>
   '}
   '
   'function getCall(s) {
   '  return s.replaceAll(/[-\<\>,]/g, \"_\");
   '}
   '
   'function modifyIdOfAllChildren(childs, i) {
   '  for (let k = 0; k \< childs.length; k++) {
   '    const c = childs[k];
   '    if (c.onchange) {
   '      const s = (\' \' + c.id).slice(1);
   '      c.onchange = function() { eval(getCall(s))(i); };
   '    }
   '    if (c.id) c.id = c.id + \"-\" + i;
   '    if (c.childNodes) modifyIdOfAllChildren(c.childNodes, i);
   '  }
   '}
  ";
}

str getComputedBody(map[str, list[str]] depends, Id var, Type t, Expr e) {
  return
  "<var>_value = <"<t>" == "integer" ? saveComputedInteger(e) : saveComputedBoolean(e)>;
  'const computed_element = document.getElementById(\"<getCompId(var.src)>\");
  'computed_element.textContent = <var>_value;
  '<"<var>" in depends ? getUpdateDependency(depends, "<var>") : empty()>";
}

str getAnswerableBody(map[str, list[str]] depends, Id var, Type t, bool b) {
  return 
  "let input;
  '<b ? getAnswerableListBody(var, t) : getAnswerableNormalBody(var, t)>
  '<"<var>" in depends ? getUpdateDependency(depends, "<var>") : empty()>";
}

str getAnswerableListBody(Id var, Type t) {
  return 
  "if (n != undefined) {
  '  input = document.getElementById(\"<getInputId(var.src)>-\" + n);
  '  <"<t>" == "integer" ? saveIntegerInList(var) : ("<t>" == "boolean" ? saveBooleanInList(var) : saveStringInList(var))>
  '}";
}

str getAnswerableNormalBody(Id var, Type t) {
  return 
  "input = document.getElementById(\"<getInputId(var.src)>\");
  '<"<t>" == "integer" ? saveInteger(var) : ("<t>" == "boolean" ? saveBoolean(var) : saveString(var))>";
}

str getIfBody(map[str, list[str]] depends, Expr e) {
  return 
  "const if_element = document.getElementById(\"<getIfId(e.src)>\");
  'if (<getExpression(e)>) {
  '  if_element.style = \"\"; 
  '  <getIfId(e.src) in depends ? getUpdateDependency(depends, getIfId(e.src)) : empty()>
  '} else {
  '  if_element.style = \"display: none;\";
  '}";
}
str getIfElseBody(map[str, list[str]] depends, Expr e) {
  return 
  "const if_element = document.getElementById(\"<getIfId(e.src)>\");
  'const else_element = document.getElementById(\"<getElseId(e.src)>\");
  'if (<getExpression(e)>) {
  '  if_element.style = \"\";
  '  else_element.style = \"display: none;\";
  '  <getIfId(e.src) in depends ? getUpdateDependency(depends, getIfId(e.src)) : empty()>
  '} else {
  '  if_element.style = \"display: none;\";
  '  else_element.style = \"\";
  '  <getElseId(e.src) in depends ? getUpdateDependency(depends, getElseId(e.src)) : empty()>
  '}";
}

str getRepeatBody(map[str, list[str]] _, Expr e) {
  return
  "const rep = <getExpression(e)>;
  'const template = document.getElementById(\"<getRepeatId(e.src)>\");
  'const parent = document.getElementById(\"<getRepeatId(e.src)>-parent\");
  'template.style = \"display: none;\"
  'let i = 0;
  'while (i \< rep) {
  '  const el = document.getElementById(\"<getRepeatId(e.src)>-\" + i);
  '  if (el == undefined) {
  '    const clone = template.cloneNode(true);
  '    clone.style = \"\";
  '    clone.id = clone.id + \"-\" + i;
  '    modifyIdOfAllChildren(clone.childNodes, i);
  '    parent.appendChild(clone);
  '  } else {
  '    el.style = \"\";
  '  }
  '  i++;
  '}
  'let el = document.getElementById(\"<getRepeatId(e.src)>-\" + i);
  'while (el != undefined) {
  '  el.style = \"display: none;\";
  '  i++;
  '  el = document.getElementById(\"<getRepeatId(e.src)>-\" + i);
  '}";

}

list[str] getFunctionCall(Question q) {
  switch(q) {
    case answerable(Str _, Id name, Type t): return [("<t>" == "boolean" ? "<name>_value = !<name>_value;" : ""), "<getSymbolNameAnswerable(name.src)>();"];
    case computed(Str _, Id name, Type _, Expr _): return ["<getSymbolNameComputed(name.src)>();"];
    case ifThen(Expr cond, Question q1): return ["<getSymbolNameIf(cond.src)>();"] + getFunctionCall(q1);
    case elseThen(Expr cond, Question q1, Question q2): return ["<getSymbolNameIf(cond.src)>();"] + getFunctionCall(q1) + getFunctionCall(q2);
    case block(Question *qs): return ( [] | it + getFunctionCall(q1) | q1 <- qs);
    case repeat(Expr e, Question q): return ["<getSymbolNameRepeat(e.src)>()"] + getFunctionCall(q);
    default: return [];
  }
}

str getUpdateDependency(map[str, list[str]] depends, str var) {
  return 
  "<for (str id <- depends[var]) {>
  '<id[0..2] == "if" ? getUpdateIfDependency(id) : getUpdateCompDependency(id)>
  '<}>";
}

str getUpdateCompDependency(str id) {
  return 
  "<id>()";
}

str getUpdateIfDependency(str id) {
  return 
  "<getSymbolNameIf(id)>();";
}

str empty() { return ""; }

str saveComputedInteger(Expr e) {
  return
  "Number(<getExpression(e)>)";
}

str saveComputedBoolean(Expr e) {
  return
  "Boolean(<getExpression(e)>)";
}

str getExpression(var(Id name)) = "<name>_value";
str getExpression(index(Id name, Expr e)) = "<name>_value[<getExpression(e)>]";
str getExpression(literal(Bool b)) = "<b>";
str getExpression(literal(Int n)) = "<n>";
str getExpression(literal(Str s)) = "<s>";
str getExpression(listing(Expr *es)) = "[ <[getExpression(e) | e <- es]> ]";
str getExpression(parentheses(Expr e)) = getExpression(e);
str getExpression(logical(Expr e)) = "!<getExpression(e)>";
str getExpression((Expr)`<Expr l> * <Expr r>`) = "<getExpression(l)> * <getExpression(r)>";
str getExpression((Expr)`<Expr l> / <Expr r>`) = "<getExpression(l)> / <getExpression(r)>";
str getExpression((Expr)`<Expr l> + <Expr r>`) = "<getExpression(l)> + <getExpression(r)>";
str getExpression((Expr)`<Expr l> - <Expr r>`) = "<getExpression(l)> - <getExpression(r)>";
str getExpression((Expr)`<Expr l> \<= <Expr r>`) = "<getExpression(l)> \<= <getExpression(r)>";
str getExpression((Expr)`<Expr l> \< <Expr r>`) = "<getExpression(l)> \< <getExpression(r)>";
str getExpression((Expr)`<Expr l> \> <Expr r>`) = "<getExpression(l)> \> <getExpression(r)>";
str getExpression((Expr)`<Expr l> \>= <Expr r>`) = "<getExpression(l)> \>= <getExpression(r)>";
str getExpression((Expr)`<Expr l> == <Expr r>`) = "<getExpression(l)> == <getExpression(r)>";
str getExpression((Expr)`<Expr l> != <Expr r>`) = "<getExpression(l)> != <getExpression(r)>";
str getExpression((Expr)`<Expr l> && <Expr r>`) = "<getExpression(l)> && <getExpression(r)>";
str getExpression((Expr)`<Expr l> || <Expr r>`) = "<getExpression(l)> || <getExpression(r)>";

str saveInteger(Id var) {
  return "<var>_value = Number(input.value);";
}

str saveBoolean(Id var) {
  return "<var>_value = !<var>_value;";
}

str saveString(Id var) {
  return "<var>_value = input.value";
}

str saveIntegerInList(Id var) {
  return "<var>_value[n] = Number(input.value)";
}

str saveBooleanInList(Id var) {
  return "<var>_value[n] = !<var>_value[n];";
}

str saveStringInList(Id var) {
  return "<var>_value[n] = input.value";
}

str getSymbolNameIf(str l) = replaceAll(replaceAll(replaceAll(replaceAll(l, "-", "_"), ",", "_"), "\<", "_"), "\>", "_");
str getSymbolNameIf(loc l) = replaceAll(replaceAll(replaceAll(replaceAll(getIfId(l), "-", "_"), ",", "_"), "\<", "_"), "\>", "_");
str getSymbolNameAnswerable(loc l) = "input_" + replaceAll(replaceAll(replaceAll(replaceAll(stripLoc(l), "-", "_"), ",", "_"), "\<", "_"), "\>", "_");
str getSymbolNameComputed(loc l) = "comp_" + replaceAll(replaceAll(replaceAll(replaceAll(stripLoc(l), "-", "_"), ",", "_"), "\<", "_"), "\>", "_");
str getSymbolNameRepeat(loc l) = "repeat_" + replaceAll(replaceAll(replaceAll(replaceAll(stripLoc(l), "-", "_"), ",", "_"), "\<", "_"), "\>", "_");



HTMLElement compile2html(start[Form] form) {
  HTMLElement scriptElement = script([]);
  scriptElement.\type = "text/javascript";
  scriptElement.src = "<stripFileLoc(form.src[extension="js"].top)>";
  HTMLElement bodyElement = body([
    h3([
      text("<form.top.title>"[1..-1])
    ])
    ] + ( [] | it + compile2html(q) | q <- form.top.questions )
  );
  bodyElement.onload = "init()";
  return html([
    lang::html::AST::head([
      scriptElement,
      title([
        text("<form.top.title>"[1..-1])
      ])
    ]),
    bodyElement
  ]);
}

HTMLElement compile2html(ifThen(Expr cond, Question then)) {
  return assignId(div([
    compile2html(then)
  ]), getIfId(cond.src));
}

HTMLElement compile2html(elseThen(Expr cond, Question ifThen, Question elseThenQuestion)) {
  return assignId(div([
    assignId(div([
      compile2html(ifThen)
    ]), getIfId(cond.src)),
    assignId(div([
      compile2html(elseThenQuestion)
    ]), getElseId(cond.src))
  ]), "if-else");
}

HTMLElement compile2html(block(Question *qs)) = assignId(div(( [] | it + compile2html(q) | q <- qs )), "block");

HTMLElement compile2html(repeat(Expr e, Question q)) = assignId(
  div([
    assignId(
      div([
        compile2html(q)
      ]),
    "<getRepeatId(e.src)>")
  ]), "<getRepeatId(e.src)>-parent"
);

HTMLElement compile2html(answerable(Str prompt, Id name, Type t)) {
  HTMLElement i = input();
  i.\type = getInputType(t);
  i.\value = getDefaultValue(t);
  i.onchange = "<getSymbolNameAnswerable(name.src)>()";
  return assignId(div([
    assignId(p([
      text("<prompt>"[1..-1])
    ]), "p"),
    assignId(i, getInputId(name.src))
  ]), "answerable");
}

HTMLElement compile2html(computed(Str prompt, Id name, Type t, Expr e)) {
  return assignId(div([
    assignId(p([
      text("<prompt>"[1..-1])
    ]), "p"),
    assignId(p([
      text(getDefaultValue(t))
    ]), getCompId(name.src))
  ]), "computed");
}

HTMLElement assignId(HTMLElement e, str id) {
  e.id = id;
  return e;
}

str getInputType(Type t) {
  switch("<t>") {
    case "integer": return "number";
    case "string": return "text";
    case "boolean": return "checkbox";
    default: return "text";
  }
}

str getDefaultValue(Type t) {
  switch("<t>") {
    case "integer": return "0";
    case "string": return "";
    case "boolean": return "false";
    default: return "";
  }
}