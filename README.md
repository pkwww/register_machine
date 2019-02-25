# register_machine
a scala inplementation of register machine, problem in UoE itcs cw1

BNF:

~~~~
input := <regSpec>\n<program>  
regSpec := registers <number>( <number>)*  
program := (<inst>\n)*  
inst := inc <register> | decjz <register> <number>  
register := r<number>  
number := [0-9]  
~~~~
