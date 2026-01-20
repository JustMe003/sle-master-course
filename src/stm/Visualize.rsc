module stm::Visualize

import vis::Graphs;
import stm::Syntax;
import Content;
import ParseTree;

alias NodeId = tuple[str name, loc src];

alias StmGraph = lrel[NodeId from, str event, NodeId to];

StmGraph stm2graph(start[Machine] m) {
    StmGraph g = [];

    map[str, loc] env
        = ( "<s.name>": s.src | /State s := m );

    for (/State s := m, Trans t <- s.transitions) {
        g += [<
                    <"<s.name>", s.src>, 
                    "<t.event>", 
                    <"<t.target>", env["<t.target>"]>
            >];
    }

    return g;
}



Content visualizeStm(start[Machine] m) = 
    graph(stm2graph(m), 
        \layout=defaultCoseLayout(), 
        nodeLabeler=str (NodeId d) { return "<d.name>"; }, 
        nodeLinker=loc (NodeId d) { return d.src; });
