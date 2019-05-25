# Variable usage
Variables are meant to be created with assignments.  
If a variable needs to be explicitly declared (eg. in the outer scope) one can use a `decl var_name` statement (pending implementation).  
There is no need to annotate a variable's type. Variable's types are inferred from their first assignments.

The transpiler will return an error if a variable with an undeterminable  type is present in the source code. Make sure that every variable is assigned to at some point.

# Expressions
Expressions consist in constants:
```
1 // int
1.2 // float
'a' // char
true // bool
"abc" // string
```
Logical operations:
`!, ==, !=, <, <=, >, >=, &&, ||`
Arithmetic operations
`+, -, *, /, %, ^(unimplemented)`
And variable identifiers.

They can have the common precedence rules, which can be overriden as needed with the help of parenthesis.
# Functions
 Functions are created on assignment with the following syntax:
```
 identifier = fun(a, b, c){
	//statements 
 }
```

Functions are treated as small independent programs, with one notable exception. Since their parameters and return type are unknown, functions depend on argument usage within the function body, or along the program.
Return types are defined by with the `return identifier` statement. A function without `return` statements doesn't return any value.  
If a function returns, then every code path must return.
# Control Flow
## Conditionals
A conditional has the following syntax:
```
if condition {
    //statements
} elif expression {
    //statements
} else {
    //statements
}

```
The `elif` can be chained as many times as needed.  
Both `elif` and `else` are optional.
## Cycles
The YGGL language supports three types of cycles.  

The `loop` variant requires no condition and loops forever:

```
loop{
    //statements
}
```
Both while and do while require an expression that can act as a condition:
```
while condition{
    //statements
}
```
```
{
    //statements
}while condition
```

The for loop will be able to perform two more optional operations than the condition checking. A variable initialization and a per-loop statement execution.  
(Albeit recognized by the language grammar the for loop is is pending an implementation)
```
for initialization; condition; statement {
    //statements
}
```

Two instructions `break` and `continue` can stop the loop and skip an interation, respectivelly. 

# Composite data types
## Structures
Structures are declared with the syntax:
```
name = struct{
    a, b, c
}
```

And defined with the syntax:
```
identifier = name{
    [a: value,]
    [b: value,]
    [c: value]
}
```
With square brackets meaning optional information. In other words, the values do not need to be initialized.

Struct attributes need to be assigned to at least once within the program source.

Attributes are accessed with the syntax: `identifier.attribute` 