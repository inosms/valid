# Introduction
Valid is a programm language, that always results in valid syntax no matter what you type (non operators are just evaluated to nothing) - though semantically it may be complete bullshit. Furthermore it is not said that any given valid program will terminate (for example `j0` will not).

Its input is a list of bitstrings and it outputs a single bitstring.

The primary usage is to provide a simple language with valid syntax for gene expression programming.

# Operators

`+` is plus
`pab` = `a + b`

`i` is if: if { evaluates to 1 at beginning } then { evaluate } else { evaluate }
`iabc` = `if a then b else c`

`t` is tail:
`ta` = `tail a` (empty results in empty)

`a` is append: 
`abc` = `b ++ c`

`p` is append positive 
`pa` = `1:a`

`n` is append negative 
`pa` = `0:a`

`r` is reverse: reverses the order of bits in the list

`e` is empty: if {evaluates to empty} then {evaluate} else {evaluate}
`eabc` = `if a is empty then b else c`

`j` is jump:
`ja..` = `jump to (evaluate a an interpret as number) with parameters (evaluate rest of gene as long as needed for parameters for destination jump gene)`

`c` is create empty:
`c` = `[]`

`0-9` is a parameter, up to now not more parameters are supported

# Usage and Examples
## Basics
Valid is parsed from left to right. To evaluate an operator the needed parameters must be evaluated. Parameters always come directly after the operator:

`t0` would apply `t` to `0`

`tp0` would apply `t`to `p`, but `p` is no value, and thus must also be evaluated, so `p0`is evaluated first and returned to `t` 

## Nothingness is empty
If the program ends but more parameters are needed, all needed parameters will evaluate to empty (`[]`)

`t` would apply `t` to nothing, and nothing always evaluates to `[]`

## Jumping
The input is always a set of programs, with each program being mapped to an index. 
e.g. executing 
```
nc
pc
```

`nc` will have index `0` and `pc` will have index `1`

With `j` one can jump to another program of the input file.

`jc` would jump to `c`, which evaluates to `[]`, which is interpreted as `0`. So the program with index `0` will be executed with nothing as parameter, resulting in an infinite loop if this program has index `0

## Example: negate
`e0ci0njct0pjct0`

this translates to:
```
if input is empty then 
	[]
else if first is 1 then
	0 : (jump 0 with parameter (tail input))
else
	1 : (jump 0 with parameter (tail input))
```

Just so easy to read, isn't it?
# Executing
To execute an file just compile the interpreter and call it with `filepath parameters`
e.g. `test.val 010 10` this will call `test.val` with the list of parameters `010` and `10`
