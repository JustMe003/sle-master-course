module Eval

import Syntax;

import ParseTree;
import IO;
import Boolean;
import String;

import lang::std::Id;


/*
 * Big-step semantics for QL
 */
 
// NB: Eval assumes the form is type- and name-correct.

// Semantic domain for expressions (values)
data Value
  = vint(int n)
  | vbool(bool b)
  | vstr(str s)
  | vlist(list[Value] l)
  ;

// The value environment, mapping question names to values.
alias VEnv = map[str name, Value \value];

// Modeling user input
data Input = user(str question, Value \value);
  

Value type2default((Type)`integer`) = vint(0);
Value type2default((Type)`string`) = vstr("");
Value type2default((Type)`boolean`) = vbool(false);
Value type2default((Type)`list[<Type _>]`) = vlist([]);


// produce an environment which for each question has a default value
// using the function type2default function defined above.
// observe how visit traverses the form and match on normal questions and computed questions.
VEnv initialEnv(start[Form] f) = initialEnv(f.top);

VEnv initialEnv(Form f) {
  VEnv venv = ();
  visit(f) {
    case answerable(Str _, Id name, Type t): venv += ("<name>": type2default(t));
    case computed(Str _, Id name, Type t, Expr e): venv += ("<name>": type2default(t));
  }

  return venv;
}

// Expression evaluation (complete for all expressions)

Value eval((Expr)`<Id x>`, VEnv venv) = venv["<x>"];
Value eval((Expr)`<Id x> [ <Expr e> ]`, VEnv venv) {
  switch(eval(e, venv)) {
    case vint(n): {
      switch(venv["<x>"][n]) {
        case vbool(b): return vbool(b);
        case vint(n): return vint(n);
        case vstr(s): return vstr(s);
        case vlist(l): return vlist(l);
        default: throw "No type found";
      }
    }
    default: throw "Expression did not resolve in an integer!";
  }
}
Value eval((Expr)`<Bool b>`, VEnv venv) = vbool(fromString("<b>"));
Value eval((Expr)`<Int i>`, VEnv venv) = vint(toInt("<i>"));
Value eval((Expr)`<Str s>`, VEnv venv) = vint(toInt("<s>"));
Value eval((Expr)`[ <Expr *es> ]`, VEnv venv) = vlist([eval(e, venv) | e <- es]);
Value eval((Expr)`( <Expr e> )`, VEnv venv) = eval(e, venv);
Value eval((Expr)`! <Expr e>`, VEnv venv) {
  switch(eval(e, venv)) {
    case vbool(b): return vbool(!b);
    default: throw "Non boolean applied to operator !";
  }
}
Value eval((Expr)`<Expr l> * <Expr r>`, VEnv venv) {
  switch(<eval(l, venv), eval(r, venv)>) {
    case <vint(nl), vint(nr)>: return vint(nl * nr);
    default: throw "Non integer applied to operator *";
  }
}
Value eval((Expr)`<Expr l> / <Expr r>`, VEnv venv) {
  switch(<eval(l, venv), eval(r, venv)>) {
    case <vint(nl), vint(nr)>: {
      if (nr == 0) {
        return vint(0);
      }
      return vint(nl / nr);
    }
    default: throw "Non integer applied to operator /";
  }
}
Value eval((Expr)`<Expr l> + <Expr r>`, VEnv venv) {
  switch(<eval(l, venv), eval(r, venv)>) {
    case <vint(nl), vint(nr)>: return vint(nl + nr);
    default: throw "Non integer applied to operator +";
  }
}
Value eval((Expr)`<Expr l> - <Expr r>`, VEnv venv) {
  switch(<eval(l, venv), eval(r, venv)>) {
    case <vint(nl), vint(nr)>: return vint(nl - nr);
    default: throw "Non integer applied to operator -";
  }
}
Value eval((Expr)`<Expr l> \< <Expr r>`, VEnv venv) {
  switch(<eval(l, venv), eval(r, venv)>) {
    case <vint(nl), vint(nr)>: return vbool(nl < nr);
    default: throw "Non integer applied to operator \<";
  }
}
Value eval((Expr)`<Expr l> \<= <Expr r>`, VEnv venv) {
  switch(<eval(l, venv), eval(r, venv)>) {
    case <vint(nl), vint(nr)>: return vbool(nl <= nr);
    default: throw "Non integer applied to operator \<=";
  }
}
Value eval((Expr)`<Expr l> \>= <Expr r>`, VEnv venv) {
  switch(<eval(l, venv), eval(r, venv)>) {
    case <vint(nl), vint(nr)>: return vbool(nl >= nr);
    default: throw "Non integer applied to operator \>=";
  }
}
Value eval((Expr)`<Expr l> \> <Expr r>`, VEnv venv) {
  switch(<eval(l, venv), eval(r, venv)>) {
    case <vint(nl), vint(nr)>: return vbool(nl > nr);
    default: throw "Non integer applied to operator \>";
  }
}
Value eval((Expr)`<Expr l> == <Expr r>`, VEnv venv) {
  switch(<eval(l, venv), eval(r, venv)>) {
    case <vint(nl), vint(nr)>: return vbool(nl == nr);
    case <vbool(bl), vbool(br)>: return vbool(bl == br);
    case <vstr(sl), vstr(sr)>: return vbool(sl == sr);
    default: throw "Non integer applied to operator ==";
  }
}
Value eval((Expr)`<Expr l> != <Expr r>`, VEnv venv) {
  switch(<eval(l, venv), eval(r, venv)>) {
    case <vint(nl), vint(nr)>: return vbool(nl != nr);
    case <vbool(bl), vbool(br)>: return vbool(bl != br);
    case <vstr(sl), vstr(sr)>: return vbool(sl != sr);
    default: throw "Non integer applied to operator !=";
  }
}
Value eval((Expr)`<Expr l> && <Expr r>`, VEnv venv) {
  switch(<eval(l, venv), eval(r, venv)>) {
    case <vbool(nl), vbool(nr)>: return vbool(nl && nr);
    default: throw "Non boolean applied to operator &&";
  }
}
Value eval((Expr)`<Expr l> || <Expr r>`, VEnv venv) {
  switch(<eval(l, venv), eval(r, venv)>) {
    case <vbool(nl), vbool(nr)>: return vbool(nl || nr);
    default: throw "Non boolean applied to operator ||";
  }
}




VEnv eval(start[Form] f, Input inp, VEnv venv) = eval(f.top, inp, venv);

// Because of out-of-order use and declaration of questions
// we use the solve primitive in Rascal to find the fixpoint of venv.
VEnv eval(Form f, Input inp, VEnv venv) {
  return solve (venv) {
    venv = evalOnce(f, inp, venv);
  }
}

// evaluate the questionnaire in one round 
VEnv evalOnce(Form f, Input inp, VEnv venv)
  = ( venv | eval(q, inp, it) | Question q <- f.questions );

VEnv eval(Question q, Input inp, VEnv venv) {
  switch(inp) {
    case user(question, v):
      switch(q) {
        case answerable(Str _, Id name, Type _): {
          if ("<question>" == "<name>") {
            venv["<name>"] = v;
          }
        }
        case computed(Str _, Id name, Type _, Expr e): {
          venv["<name>"] = eval(e, venv);
        }
        case ifThen(Expr e, Question then): {
          Value val = eval(e, venv);
          switch(val) {
            case vbool(b): {
              if (b) venv = eval(then, inp, venv);
            }
          }
        }
        case elseThen(Expr e, Question then, Question elseThen): {
          Value val = eval(e, venv);
          switch(val) {
            case vbool(b): {
              if (b) venv = eval(then, inp, venv);
              else venv = eval(elseThen, inp, venv);
            }
          }
        }
        case block(Question *qs): {
          venv = (venv | eval(blockQ, inp, it) | blockQ <- qs);
        }
        case repeat(Expr e, Question rep): {
          Value val = eval(e, venv);
          switch(val) {
            case vint(n): {
              if (n > 0) venv = eval(rep, inp, venv);
            }
          }
        }
      }
  }
  return venv;
}

/*
 * Rendering UIs: use questions as widgets
 */

list[Question] render(start[Form] form, VEnv venv) = render(form.top, venv);

list[Question] render(Form form, VEnv venv) = ( [] | concat([it, render(q, venv)]) | q <- form.questions );
list[Question] render(ifThen(Expr e, Question q), VEnv venv) {
  switch(eval(e, venv)) {
    case vbool(b): {
      if (b) {
        return render(q, venv);
      }
    }
  }
  return [];
}
list[Question] render(elseThen(Expr cond, Question ifThen, Question elseThen), VEnv venv) {
  switch(eval(cond, venv)) {
    case vbool(b): {
      if (b) {
        return render(ifThen, venv);
      } else {
        return render(elseThen, venv);
      }
    }
  }
  return [];
}
list[Question] render(block(Question *qs), VEnv venv) = ( [] | concat([it, render(q, venv)]) | q <- qs );

list[Question] render(Question q, VEnv _) = [q];

Expr value2expr(vbool(bool b)) = [Expr]"<b>";
Expr value2expr(vstr(str s)) = [Expr]"\"<s>\"";
Expr value2expr(vint(int i)) = [Expr]"<i>";

void printUI(list[Question] ui) {
  for (Question q <- ui) {
    println(q);
  }
}


void evalSnippets() {
  start[Form] pt = parse(#start[Form], |project://sle-master-course/examples/tax.myql|);

  env = initialEnv(pt);
  env2 = eval(pt, user("hasSoldHouse", vbool(true)), env);
  env3 = eval(pt, user("sellingPrice", vint(1000)), env2);
  env4 = eval(pt, user("privateDebt", vint(500)), env3);

  for (Input u <- [user("hasSoldHouse", vbool(true)), user("sellingPrice", vint(1000)), user("privateDebt", vint(500))]) {
    printUI(render(pt, env));
    env = eval(pt, u, env);
    // println(env);
  }
  printUI(render(pt, env));
}