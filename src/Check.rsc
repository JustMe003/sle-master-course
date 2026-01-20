module Check

import Message;
import IO;
import ParseTree;
import List;
import Set;
import String;
import ListRelation;

import lang::std::Id;

extend Syntax;

// internal type to represent unknown 
syntax Type = "*unknown*";

// type environment maps question names to types
// (NB: it's not a map, because the form can contain errors!)
alias TEnv = lrel[str, Type];

// build a Type Environment (TEnv) for a questionnaire.


TEnv collect(Form f) = [ <"<x>", t> | /answerable(Str _, Id x, Type t) := f ] + [ <"<x>", t> | /computed(Str _, Id x, Type t, Expr _) := f ];

/*
 * typeOf: compute the type of expressions
 */

Type typeOf(literal(Bool _), TEnv env) = (Type)`boolean`;
Type typeOf(literal(Int _), TEnv env) = (Type)`integer`;
Type typeOf(literal(Str _), TEnv env) = (Type)`string`;
Type typeOf(parentheses(Expr e), TEnv env) = typeOf(e, env);
Type typeOf(logical(Expr e), TEnv env) {
    if ("<typeOf(e, env)>" == "boolean") {
        return (Type)`boolean`;
    }
    return (Type)`*unknown*`;
}
Type typeOf(logical(Expr l, Expr r), TEnv env) {
    if ("<typeOf(l, env)>" == "boolean" && "<typeOf(r, env)>" == "boolean") {
        return (Type)`boolean`;
    }
    return (Type)`*unknown*`;
}
Type typeOf(relational(Expr l, Expr r), TEnv env) {
    if ("<typeOf(l, env)>" == "integer" && "<typeOf(r, env)>" == "integer") {
        return (Type)`boolean`;
    }
    return (Type)`*unknown*`;
}
Type typeOf(arithmetic(Expr l, Expr r), TEnv env) {
    if ("<typeOf(l, env)>" == "integer" && "<typeOf(r, env)>" == "integer") {
        return (Type)`integer`;
    }
    return (Type)`*unknown*`;
}

// the fall back type is *unknown*
default Type typeOf(Expr _, TEnv env) = (Type)`*unknown*`;

// a reference has the type of its declaration
Type typeOf((Expr)`<Id x>`, TEnv env) = t
    when <"<x>", Type t> <- env;

/*
 * Checking forms
 */

set[Message] check(start[Form] form) = check(form.top);

set[Message] check(Form form) 
  = { *check(q, env) | Question q <- form.questions }
  + checkDuplicates(form)
  + checkCycles(form)
  when TEnv env := collect(form);

set[Message] check(ifThen(Expr e, Question q), TEnv env) {
    set[Message] s = {};
    Type t = typeOf(e, env);
    s += check(e, env);
    if ("<t>" != "boolean" && "<t>" != "*unknown*") s += {error("Conditions are type checked", e.src)};
    if ("<e>" == "true") s += {warning("useless conditional", e.src)};
    elseif ("<e>" == "false") s += {warning("Dead if branch", e.src)};
    return s + check(q, env);
}
set[Message] check(elseThen(Expr e, Question q1, Question q2), TEnv env) {
    set[Message] s = {};
    Type t = typeOf(e, env);
    s += check(e, env);
    if ("<t>" != "boolean" && "<t>" != "*unknown*") s += {error("Conditions are type checked", e.src)};
    if ("<e>" == "true") s += {warning("Dead else branch", e.src)};
    elseif ("<e>" == "false") s += {warning("Dead if branch", e.src)};
    return s + check(q1, env) + check(q2, env);
}
set[Message] check(block(Question* qs), TEnv env) = (0 | it + 1 | _ <- qs) == 0 ? 
    {warning("Empty branch", qs.src)} : 
    ( {} | check(q, env) | q <- qs);
set[Message] check(computed(Str prompt, Id name, Type t, Expr e), TEnv env) {
    set[Message] s = {};
    if (size("<prompt>") <= 2) s += {warning("Empty prompt", prompt.src)};
    s += check(e, env);
    if ("<t>" != "<typeOf(e, env)>" && "<typeOf(e, env)>" != "*unknown*") s += {error("Expression does not match declared type", e.src)};
    return s;
}
set[Message] check(answerable(Str prompt, Id name, Type t), TEnv env) {
    set[Message] s = {};
    if (size("<prompt>") <= 2) s += {warning("Empty prompt", prompt.src)};

    return s;
}

set[Message] check(var(Id name), TEnv env) {
    set[Message] s = {};
    println("<name>" in domain(env));
    if ("<name>" notin domain(env) || <"<name>", (Type)`*unknown*`> in env) s += {error("undefined reference", name.src)};
    return s;
}
set[Message] check(parentheses(Expr e), TEnv env) = check(e, env);
set[Message] check(logical(Expr e), TEnv env) =
    (("<typeOf(e, env)>" != "boolean" && "<typeOf(e, env)>" != "*unknown*") 
    ? {error("Invalid operant to !", e.src)} 
    : {}) 
    + check(e, env);

set[Message] check(arithmetic(Expr l, Expr r), TEnv env) =
    (("<typeOf(l, env)>" != "integer" && "<typeOf(l, env)>" != "*unknown*")
    ? {error("Invalid operant to arithmetic operator", l.src)}
    : {})
    + (("<typeOf(r, env)>" != "integer" && "<typeOf(r, env)>" != "*unknown*")
    ? {error("Invalid operant to arithmetic operator", r.src)}
    : {})
    + check(l, env) + check(r, env);
set[Message] check(relational(Expr l, Expr r), TEnv env) =
    (("<typeOf(l, env)>" != "integer" && "<typeOf(l, env)>" != "*unknown*")
    ? {error("Invalid operant to relation operator", l.src)}
    : {})
    + (("<typeOf(r, env)>" != "integer" && "<typeOf(r, env)>" != "*unknown*")
    ? {error("Invalid operant to relation operator", r.src)}
    : {})
    + check(l, env) + check(r, env);
set[Message] check(logical(Expr l, Expr r), TEnv env) =
    (("<typeOf(l, env)>" != "integer" && "<typeOf(l, env)>" != "*unknown*")
    ? {error("Invalid operant to logical operator", l.src)}
    : {})
    + (("<typeOf(r, env)>" != "integer" && "<typeOf(r, env)>" != "*unknown*")
    ? {error("Invalid operant to logical operator", r.src)}
    : {})
    + check(l, env) + check(r, env);


set[Message] validExpression(var(Id name), set[tuple[str, str]] symbols) {
    println(symbols);
    if (<"<name>", "integer"> notin symbols && <"<name>", "boolean"> notin symbols && <"<name>", "string"> notin symbols) {
        return {error("Variable `<name>` was never declared or its type is unknown. Possible cyclic data dependency", name.src)};
    }
    return {};
}
set[Message] validExpression(parentheses(Expr e), set[tuple[str, str]] symbols) = validExpression(e, symbols);
set[Message] validExpression(logical(Expr e), set[tuple[str, str]] symbols) = validExpression(e, symbols);
set[Message] validExpression(logical(Expr l, Expr r), set[tuple[str, str]] symbols) = validExpression(l, symbols) + validExpression(r, symbols);
set[Message] validExpression(arithmetic(Expr l, Expr r), set[tuple[str, str]] symbols) = validExpression(l, symbols) + validExpression(r, symbols);
set[Message] validExpression(relational(Expr l, Expr r), set[tuple[str, str]] symbols) = validExpression(l, symbols) + validExpression(r, symbols);

set[Message] validExpression(Expr _, set[tuple[str, str]] symbols) = {};

set[Message] checkCycles(Form form) {
    set[Message] s = {};

    set[tuple[str, str]] symbols = {};

    visit(form) {
        case answerable(Str _, Id name, Type t): symbols += {<"<name>", "<t>">};
        case computed(Str _, Id name, Type t, Expr e): {
            set[Message] new = validExpression(e, symbols);
            if (isEmpty(new)) {
                symbols += {<"<name>", "<t>">};
            } else {
                symbols += {<"<name>", "<(Type)`*unknown*`>">};
            }
            s += new;
        }
        case ifThen(Expr e, Question _): {
            s += validExpression(e, symbols);
        }
        case elseThen(Expr e, Question _, Question _): {
            s += validExpression(e, symbols);
        }
    }

    return s;
}

set[Message] checkDuplicates(Form form) {
    set[Message] s = {};
    list[tuple[str prompt, str name, Type t]] names = [];
    visit(form) {
        case answerable(Str prompt, Id name, Type t): {
            for (element <- names) {
                if ("<name>" == element.name) {
                    if ("<t>" != "<element.t>") s += {error("Redeclared with different type", name.src)};
                    elseif ("<prompt>" != element.prompt) s += {warning("Redeclared with different prompt", prompt.src)};
                }
                if ("<prompt>" == element.prompt) s += {warning("duplicate labels", prompt.src)};
            }
            names = push(<"<prompt>", "<name>", t>, names);
        }
        case computed(Str prompt, Id name, Type t, Expr _): {
            for (element <- names) {
                if ("<name>" == element.name) {
                    if ("<t>" != "<element.t>") s += {error("Redeclared with different type", name.src)};
                    elseif ("<prompt>" != element.prompt) s += {error("Redeclared with different prompt", prompt.src)};
                }
                if ("<prompt>" == element.prompt) s += {warning("duplicate labels", prompt.src)};
            }
            push(<prompt, name, t>, names);
        }
    }
    return s;
}

/*
 * Checking questions
 */

// by default, there are no errors or warnings
default set[Message] check(Question _, TEnv _) = {};


/*
 * Checking expressions
 */


// when the other cases fail, there are no errors
default set[Message] check(Expr _, TEnv env) = {};

set[Message] check(e:(Expr)`<Id x>`, TEnv env) = {error("undefined question", x.src)}
    when "<x>" notin env<0>;

set[Message] check((Expr)`(<Expr e>)`, TEnv env) = check(e, env);


void printTEnv(TEnv tenv) {
    for (<str x, Type t> <- tenv) {
        println("<x>: <t>");
    }
}

