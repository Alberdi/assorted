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


## whereami.py

"WHERE AM I D:" is a small game I did in 0 hours for the [0h Game Jam](http://0hgame.eu/) 2012. Made in Python with the help of the lovely [libtcod](http://doryen.eptalys.net/libtcod/).

Move yourself with the arrow keys. When you think you know where you are point the cursor and press spacebar (clicking was throwing me a Segmentation fault error and I did not have time to fix it). You can exit at (almost) anytime with the escape key.

You can get a ready to play Linux version [here](https://github.com/downloads/Alberdi/assorted/whereami-LINUX.tar.gz).