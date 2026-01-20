package stm;
 
public class Doors {

   
   private final static int closed = 0;
   
   private final static int current = 1;
   
   private final static int locked = 2;
   

   private int $current = 0;

   public void run(java.util.List<String> $input) {
     for (String $event: $input) {
       
       if ($current == closed) {
         
         if ($event.equals("open")) {
           $current = current;
           continue;
         }
        
         if ($event.equals("lock")) {
           $current = locked;
           continue;
         }
        
       }
      
       if ($current == current) {
         
         if ($event.equals("close")) {
           $current = closed;
           continue;
         }
        
       }
      
       if ($current == locked) {
         
         if ($event.equals("unlock")) {
           $current = closed;
           continue;
         }
        
       }
      
     }
   }  
}