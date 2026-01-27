module Visualize

import vis::Graphs;
import Syntax;
import Content;
import ParseTree;

import lang::std::Id;

// Identity of a node
alias DepId = tuple[Id name, loc location];

alias DepGraph = lrel[DepId from, str kind, DepId to];


Content visualizeDeps(start[Form] form) = 
    graph(form2deps(form), 
        \layout=defaultCoseLayout(), 
        nodeLabeler=str (DepId d) { return "<d.name>"; }, 
        nodeLinker=loc (DepId d) { return d.location; });


DepGraph form2deps(start[Form] f) = form2deps(f.top);

// extra control/data dependencies from a form
// use the kind field in DepGraph to indicate whether it's a data dependency or a control dependency.
DepGraph form2deps(Form f) {
    DepGraph g = [];
    map[str, DepId] m = ();
    visit(f.questions) {
        case answerable(Str _, Id name, Type _): m["<name>"] = <name, name.src>;
        case computed(Str _, Id name, Type _, Expr e): { DepId d = <name, name.src>; m["<name>"] = d; g += [<d2, "data", d> | d2 <- getDependencies(m, e)]; }
        case ifThen(Expr cond, Question q): g += [<d1, "control", d2> | d1 <- getDependencies(m, cond), d2 <- getNextQuestions(q) ];
        case elseThen(Expr cond, Question q1, Question q2): g += [<d1, "control", d2> | d1 <- getDependencies(m, cond), d2 <- (getNextQuestions(q1) + getNextQuestions(q2))];
        case repeat(Expr e, Question q): g += [<d1, "control", d2> | d1 <- getDependencies(m, e), d2 <- getNextQuestions(q)];
    }
    return g;
}

set[DepId] getNextQuestions(answerable(Str _, Id name, Type _)) = {<name, name.src>};
set[DepId] getNextQuestions(computed(Str _, Id name, Type _, Expr _)) = {<name, name.src>};
set[DepId] getNextQuestions(ifThen(Expr _, Question q)) = getNextQuestions(q); 
set[DepId] getNextQuestions(elseThen(Expr _, Question q1, Question q2)) = getNextQuestions(q1) + getNextQuestions(q2);
set[DepId] getNextQuestions(block(Question *qs)) = ( {} | it + getNextQuestions(q) | q <- qs );
set[DepId] getNextQuestions(repeat(Expr _, Question q)) = getNextQuestions(q); 


set[DepId] getDependencies(map[str, DepId] m, var(Id name)) = {m["<name>"]};
set[DepId] getDependencies(map[str, DepId] m, parentheses(Expr e)) = getDependencies(m, e);
set[DepId] getDependencies(map[str, DepId] m, logical(Expr e)) = getDependencies(m, e);
set[DepId] getDependencies(map[str, DepId] m, arithmetic(Expr l, Expr r)) = getDependencies(m, l) + getDependencies(m, r);
set[DepId] getDependencies(map[str, DepId] m, logical(Expr l, Expr r)) = getDependencies(m, l) + getDependencies(m, r);
set[DepId] getDependencies(map[str, DepId] m, relational(Expr l, Expr r)) = getDependencies(m, l) + getDependencies(m, r);
set[DepId] getDependencies(map[str, DepId] m, Expr e) = {};

