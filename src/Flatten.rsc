module Flatten

import Syntax;
import ParseTree;

extend lang::std::Id;

/* Normalization:
 *  wrt to the semantics of QL the following
 *     q0: "" int; 
 *     if (a) { 
 *        if (b) { 
 *          q1: "" int; 
 *        } 
 *        q2: "" int; 
 *      }
 *
 *  is equivalent to
 *     if (true) q0: "" int;
 *     if (true && a && b) q1: "" int;
 *     if (true && a) q2: "" int;
 *
 * Write a transformation that performs this flattening transformation.
 *
 */
 
list[Question] flatten(start[Form] f) = flatten(f.top);

list[Question] flatten(Form f) = ( [] | it + flattenQuestions(q, []) | q <- f.questions );

list[Question] flatten(repeat(Expr _, Question q)) = flatten(q);

list[Question] flattenQuestions(ifThen(Expr cond, Question then), list[Expr] expressions) {
    expressions += [cond];
    return flattenQuestions(then, expressions);
}

list[Question] flattenQuestions(elseThen(Expr cond, Question ifThen, Question elseThen), list[Expr] expressions) {
    list[Expr] ifExprs = expressions + [cond];
    list[Question] l = flattenQuestions(ifThen, ifExprs);
    list[Expr] elseExprs = expressions + [reverseExpr(cond)];
    return flattenQuestions(elseThen, elseExprs) + l;
}

list[Question] flattenQuestions(block(Question *qs), list[Expr] expressions) = ( [] | it + flattenQuestions(q, expressions) | q <- qs );

list[Question] flattenQuestions(answerable(Str prompt, Id name, Type t), list[Expr] expressions) {
    Expr cond = concatExpressions(expressions);
    return [parse(#Question, "if (<cond>) <prompt> <name>: <t>")];
}

list[Question] flattenQuestions(computed(Str prompt, Id name, Type t, Expr e), list[Expr] expressions) {
    Expr cond = concatExpressions(expressions);
    return [parse(#Question, "if (<cond>) <prompt> <name>: <t> = <e>")];
}

Expr concatExpressions(list[Expr] exprs) = ( (Expr)`true` | parse(#Expr, "<it> && <e>") | e <- exprs );



Expr reverseExpr(Expr e) {
    switch(e) {
        case literal(Bool b): return ("<b>" == "true" ? (Expr)`false` : (Expr)`true`);
        case (Expr)`<Expr l> \< <Expr r>`: return (Expr)`<Expr l> \>= <Expr r>`;
        case (Expr)`<Expr l> \<= <Expr r>`: return (Expr)`<Expr l> \> <Expr r>`; 
        case (Expr)`<Expr l> \>= <Expr r>`: return (Expr)`<Expr l> \< <Expr r>`; 
        case (Expr)`<Expr l> \> <Expr r>`: return (Expr)`<Expr l> \<= <Expr r>`; 
        case (Expr)`<Expr l> == <Expr r>`: return (Expr)`<Expr l> != <Expr r>`; 
        case (Expr)`<Expr l> != <Expr r>`: return (Expr)`<Expr l> == <Expr r>`;
        case (Expr)`<Expr l> && <Expr r>`: {
            Expr l2 = reverseExpr(l);
            Expr r2 = reverseExpr(r);
            return (Expr)`<Expr l2> && <Expr r2>`;
        }
        case (Expr)`<Expr l> || <Expr r>`: {
            Expr l2 = reverseExpr(l);
            Expr r2 = reverseExpr(r);
            return (Expr)`<Expr l2> || <Expr r2>`;
        }
        default: return literal((Bool)`true`);
    }
}

// helper function to go back to a proper questionnaire term.
start[Form] unflatten(list[Question] qs, start[Form] org) {
    Str title = org.top.title;
    Form f = (Form)`form <Str title> {}`;
    for (Question q <- qs, (Form)`form <Str t> {<Question* qqs>}` := f) {
        f = (Form)`form <Str t> {
                  '  <Question* qqs>
                  '  <Question q>
                  '}`;
    }
    return org[top=f];
}
