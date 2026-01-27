module Resolve

import Syntax;
import ParseTree;
import IO;

/*
 * Name resolution for QL
 */ 


// modeling declaring occurrences of names
alias Def = rel[str name, loc def];

// modeling use occurrences of names
alias Use = rel[loc use, str name];

alias UseDef = rel[loc use, loc def];

// the reference graph
alias RefGraph = tuple[
  Use uses, 
  Def defs, 
  UseDef useDef
];

RefGraph resolve(start[Form] f) = <us, ds, us o ds>
  when Use us := uses(f), Def ds := defs(f);

Use uses(start[Form] f) {
  Use u = {};
  for (/var(Id name) := f) {
    u += {<name.src, "<name>">};
  }
  for (/index(Id name, Expr _) := f) {
    u += {<name.src, "<name>">};
  }
  return u;
}

Def defs(start[Form] f) {
  Def d = {};
  for (/answerable(Str _, Id name, Type _) := f) {
    d += {<"<name>", name.src>};
  }
  for (/computed(Str _, Id name, Type _, Expr _) := f) {
    d += {<"<name>", name.src>};
  }
  return d;
}