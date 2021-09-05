# CS4012 Supplemental - Luka Hickey

## -Using the Interpreter-
The interpreter can be run using the *run* function on a Program, an additional type of Statement was added - Condbreak - which can be added to a program by the user to inputting a statement like the below example from a sample program prog10: 

`condbreak ("scratch" .> (5::Int))`

The above snippet when read by the interpreter adds the boolean expression `"scratch" .> (5::Int)` to a list maintained by the Run monad, the contents of that list are subsequently evalutated after every statement execution and upon any of the expressions returning as False the program execution is halted.

Additional to this case : before the execution of every statement the program is also halted. 

When the program is halted users are presented with a UI in the console informing the user of the next statement execution and of their available inputs: 
```
condbreak (scratch .> 5)  <-
-------------------------------------------------------------------
Program execution halted, please input one of the following options:
'next' - To execute the next statement
'list' - To show the current list of variables
'print <var>': To print the value of a variable of your choice
-------------------------------------------------------------------
>
```
