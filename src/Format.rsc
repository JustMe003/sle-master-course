module Format

import util::SimpleBox;
import Syntax;

import lang::std::Id;

/*
 * Formatting: transforming QL forms to Box 
 */

str formatQL(start[Form] form) = format(form2box(form));

Box form2box(start[Form] form) = form2box(form.top);

Box form2box(Form form) = V(
    H("<form.title>"),
    I([ form2box(q) | q <- form.questions ])
);

Box form2box(answerable(Str prompt, Id name, Type t)) = H("<prompt>", "<name>:", "<t>", hs=1);
Box form2box(computed(Str prompt, Id name, Type t, Expr e)) = H("<prompt>", "<name>", "<t> =", form2box(e), hs=1);
Box form2box(ifThen(Expr cond, Question q)) = V(
    H("if (", form2box(cond), ")"),
    I(form2box(q))
);
Box form2box(elseThen(Expr cond, Question q1, Question q2)) = V(
    H("if (", form2box(cond), ")"), 
    I(form2box(q1)),
    "else",
    I(form2box(q2))
);
Box form2box(block(Question *qs)) = V("{", I([form2box(q) | q <- qs]),"}");
Box form2box(repeat(Expr e, Question q)) = V(
    H("repeat (", form2box(e), ")"),
    I(form2box(q))
);

Box form2box(var(Id name)) = H("<name>");
Box form2box(index(Id name, Expr e)) = H("<name>", "[", form2box(e), "]");
Box form2box(literal(Bool b)) = H("<b>");
Box form2box(literal(Int n)) = H("<n>");
Box form2box(literal(Str s)) = H("<s>");
Box form2box(listing(Expr *exprs)) = H("[", [ form2box(e) | e <- exprs ], "]");
Box form2box(parentheses(Expr e)) = H("(", form2box(e), ")");
Box form2box(logical(Expr e)) = H("!", form2box(e));
Box form2box((Expr)`<Expr l> * <Expr r>`) = H(form2box(l), "*", form2box(r), hs=1);
Box form2box((Expr)`<Expr l> / <Expr r>`) = H(form2box(l), "/", form2box(r), hs=1);
Box form2box((Expr)`<Expr l> + <Expr r>`) = H(form2box(l), "+", form2box(r), hs=1);
Box form2box((Expr)`<Expr l> - <Expr r>`) = H(form2box(l), "-", form2box(r), hs=1);
Box form2box((Expr)`<Expr l> \< <Expr r>`) = H(form2box(l), "\<", form2box(r), hs=1);
Box form2box((Expr)`<Expr l> \<= <Expr r>`) = H(form2box(l), "\<=", form2box(r), hs=1);
Box form2box((Expr)`<Expr l> \>= <Expr r>`) = H(form2box(l), "\>=", form2box(r), hs=1);
Box form2box((Expr)`<Expr l> \> <Expr r>`) = H(form2box(l), "\>", form2box(r), hs=1);
Box form2box((Expr)`<Expr l> == <Expr r>`) = H(form2box(l), "==", form2box(r), hs=1);
Box form2box((Expr)`<Expr l> != <Expr r>`) = H(form2box(l), "!=", form2box(r), hs=1);
Box form2box((Expr)`<Expr l> && <Expr r>`) = H(form2box(l), "&&", form2box(r), hs=1);
Box form2box((Expr)`<Expr l> || <Expr r>`) = H(form2box(l), "||", form2box(r), hs=1);

