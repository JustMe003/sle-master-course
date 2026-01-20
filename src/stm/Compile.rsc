module stm::Compile

import stm::Syntax;



str compile0(start[Machine] m) = compile(m.top);

str compile(Machine m, int i = -1) 
    = "package stm;
      ' 
      'public class <m.name> {
      '
      '   <for (State s <- m.states) { i += 1; >
      '   private final static int <s.name> = <i>;
      '   <}>
      '
      '   private int $current = 0;
      '
      '   public void run(java.util.List\<String\> $input) {
      '     for (String $event: $input) {
      '       <for (State s <- m.states) {>
      '       if ($current == <s.name>) {
      '         <for (Trans t <- s.transitions) {>
      '         if ($event.equals(\"<t.event>\")) {
      '           $current = <t.target>;
      '           continue;
      '         }
      '        <}>
      '       }
      '      <}>
      '     }
      '   }  
      '}";



