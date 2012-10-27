## celauto.ml

[Elementary cellular automaton](https://en.wikipedia.org/wiki/Elementary_cellular_automaton) made in OCaml.

After being compiled, it can be run as follows:
`celauto rule states iterations`.
* `rule` is a number from 0 to 255 that gives the formula needed to calculate the next iteration.
* `states` is a string that specifies the initial state of the automaton. The characters ' ', '.', '0' and 'E' are cells with a 0, meanwhile any other character is a 1.
* `iterations` is the number of times that the simulation will be run.
* The output is a number of lines equal to `iterations` printed in the console that correspond with the consecutive states derived from the first given. Here the blank character symbolizes 0 and a @ character is 1.

Example of use: `celauto 90 EEEEEEEEEEEEEEEFEEEEEEEEEEEEEEE 16`.

Output of the example:

                   @               
                  @ @              
                 @   @             
                @ @ @ @            
               @       @           
              @ @     @ @          
             @   @   @   @         
            @ @ @ @ @ @ @ @        
           @               @       
          @ @             @ @      
         @   @           @   @     
        @ @ @ @         @ @ @ @    
       @       @       @       @   
      @ @     @ @     @ @     @ @  
     @   @   @   @   @   @   @   @ 
    @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @
