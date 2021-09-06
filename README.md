# CS4012 Supplemental - Luka Hickey

## -Using the Interpreter-
The interpreter can be run using the *run* function on a Program, an additional type of Statement was added - Condbreak - which can be added to a program by the user by inputting a statement like the below example from a sample program prog10: 

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
## Section 2
Run monad modified to step through program. This was done by taking and acting on input with a `userprompt` function before a step action is called from `step (Seq s0 s1)`.
## Section 3
`run` was modified to prompt the user interactively by handling IO within the step action, the variable environment is accessed within Run monad actions to allow users to print variables and list all variables at any point in program execution.
## Section 4
Interpreter extended to allow multiple conditional breakpoints, they can be entered into a program using the Condbreak Statement, which upon execution appends the expression to an expression list stored in a tuple maintained by the State monad within Run. After each statement execution the expressions are evaluated in the `checkbreakpoints` action, and if any of them evaluate to a Boolean False value then program execution is halted allowing variable inspection.
## Section 5
None completed.
