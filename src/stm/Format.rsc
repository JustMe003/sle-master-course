module stm::Format

import stm::Syntax;
import util::SimpleBox;



Box stm2box(start[Machine] m) = stm2box(m.top);

Box stm2box((Machine)`machine <Id n> <State* ss> end`)
 = V(
    H("machine", "<n>",hs=1),
    I([ stm2box(s) | State s <- ss], vs=2),
    "end"
 );

 Box stm2box((State)`state <Id s> <Trans* ts> end`)
 = V(
    H("state", "<s>", hs=1),
    I([ stm2box(t) | Trans t <- ts]),
    "end"
 );

 Box stm2box((Trans)`<Id e> =\> <Id t>`)
    = H("<e>", "=\>", "<t>", hs=1);