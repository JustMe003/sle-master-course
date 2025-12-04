module stm::Exec

import stm::Syntax;

str initialState(start[Machine] m) = "<s.name>"
    when State s <- m.top.states;

alias Env = map[str, State];

Env stateEnv(start[Machine] m) 
    = ( "<s.name>": s | State s <- m.top.states );


str exec(start[Machine] machine, list[str] events, str current=initialState(machine)) {
    Env env = stateEnv(machine);
    for (str event <- events) {
        State s = env[current];
        <fired, current> = exec(event, s);
        if (!fired) {
           current = initialState(machine);
        }    
    }
    return current;
}

tuple[bool, str] exec(str event, State s) {
    bool fired = false;
    str current = "";
    for (Trans t <- s.transitions) {
        if ("<t.event>" == event) {
            current = "<t.target>";
            fired = true;
        }
    }
    return <fired, current>;
}
    