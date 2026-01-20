module stm::App

import salix::HTML;
import salix::App;
import salix::Core;
import salix::Index;

import stm::Syntax;
import stm::Exec;
import String;

alias Model = tuple[start[Machine] machine, str current];

data Msg = fire(str event);

Model update(Msg msg, Model m) {
    switch (msg) {
        case fire(str event): {
            m.current = exec(m.machine, [event], current=m.current);   
        }
    }
    return m;
}

void view(Model m) {
    h3("State machine: <m.machine.top.name>");
    p("Current state: <m.current>");

    ul(() {
        for (State s <- m.machine.top.states) {
            li(() {
                p("<s.name>");

                for (Trans t <- s.transitions) {
                    if ("<s.name>" == m.current) {
                        button(onClick(fire("<t.event>")), "<t.event>");
                    }
                    else {
                        button(disabled(true), "<t.event>");
                    }
                }

            });
        }
    });



}













App[Model] runStm(start[Machine] stm) = webApp(stmApp(stm), |project://sle-master-course/src|);

SalixApp[Model] stmApp(start[Machine] stm, str id="root") 
  = makeApp(id, 
        Model() { return <stm, initialState(stm)>; }, 
        withIndex("<stm.top.name>", id, view
        // , css=["https://cdn.simplecss.org/simple.min.css"]
        ), 
        update);

