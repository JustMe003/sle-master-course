module Syntax

extend lang::std::Layout;
extend lang::std::Id;

start syntax Form 
  = form: "form" Str title "{" Question* questions "}"; 

lexical Str = [\"]![\"]* [\"];

lexical Int = [0-9]+; 

lexical Bool = "true" | "false";

// boolean, integer, string
syntax Type 
  = "boolean"
  | "integer"
  | "string"
  | "list[" Type "]"
  ;


// TODO: answerable question, computed question, block, if-then-else
syntax Question
  = ifThen: "if" "(" Expr cond ")" Question then !>> "else"
  | elseThen: "if" "(" Expr cond ")" Question ifThen "else" Question elseThen 
  | answerable: Str Id name ":" Type !>> "="
  | computed: Str Id name ":" Type "=" Expr
  | block: "{" Question* questions "}"
  | repeat: "repeat" "(" Expr e ")" Question q
  ;

// TODO: +, -, *, /, &&, ||, !, >, <, <=, >=, ==, !=, literals (bool, int, str)
// Think about disambiguation using priorities and associativity
// and use C/Java style precedence rules (look it up on the internet)
syntax Expr
  = var: Id name \ "true" \ "false"
  | index: Id name "[" Expr e "]"
  | literal: Bool bool
  | literal: Int int
  | literal: Str str
  | listing: "[" Expr* exprs "," "]"
  | parentheses: "(" Expr e ")"
  > logical: "!" Expr e
  > left (arithmetic: Expr l "*" Expr r
    | arithmetic: Expr l "/" Expr r)
  > left (arithmetic: Expr l "+" Expr r
    | arithmetic: Expr l "-" Expr r)
  > left (relational: Expr l "\<" Expr r
    | relational: Expr l "\<=" Expr r
    | relational: Expr l "\>=" Expr r
    | relational: Expr l "\>" Expr r)
  > left (relational: Expr l "==" Expr r
    | relational: Expr l "!=" Expr r)
  > left logical: Expr l "&&" Expr r
  > left logical: Expr l "||" Expr r
  ;

