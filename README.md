# fluent
This an interpreter for dynamically typed,
functional, immutable language 'fluent'.
Currently it only supports a subset of the language,
but it allows for some simple programs.

There is also a [vscode extension](https://github.com/dazzlemon/fluent-vsc).

following snippet prints first 9 numbers of fibonacci sequence:
```
fib <- (n) {
  match n {
    0 -> 0;
    1 -> 1;
    _ -> add(fib(sub(n 1)) fib(sub(n 2)));
  };
};

fromTo <- (i n x) {
	x(i);
	match i {
		n -> NULL;
		_ -> fromTo(add(i 1) n x);
	};
};

printFib <- (n) {
	print(fib(n));
};

fromTo(0 8 printFib);
```

```
0
1
1
2
3
5
8
13
21
```

# Compile & Use
You'll need to
<a href="https://docs.haskellstack.org/en/stable/README/#how-to-install" target="_blank" rel="noreferrer noopener">install stack</a>.
When in project's repository:
```bash
# this will compile the project
stack build

# you can extract exe and use it separately
stack path --local-install-root# this will show where you can find built exe
# e.g this returns ~/github/fluent/.stack-work/install/x86_64-linux-tinfo6/026e5ea1a4d73b5051b30008ef9d7bb4db7a2d25ce9db4b6c46b70d05993998d/9.0.2
# this means exe is at ~/github/fluent/.stack-work/install/x86_64-linux-tinfo6/026e5ea1a4d73b5051b30008ef9d7bb4db7a2d25ce9db4b6c46b70d05993998d/9.0.2/bin/fluent-exe

fluent-exe fib.fluent

# or you can use it with stack
stack exec -- fluent-exe fib.fluent
```

Or you can download latest precompiled executable
for your system in releases tab.

# Syntax

Syntax of the language is pretty straight forward, here it is described in ebnf:

## EBNF
```
digit ::= '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'

alphaL ::= 'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g'
         | 'h' | 'i' | 'j' | 'k' | 'l' | 'm' | 'n'
         | 'o' | 'p' | 'q' | 'r' | 's' | 't' | 'u'
         | 'v' | 'w' | 'x' | 'y' | 'z'

alphaU ::= 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G'
         | 'H' | 'I' | 'J' | 'K' | 'L' | 'M' | 'N'
         | 'O' | 'P' | 'Q' | 'R' | 'S' | 'T' | 'U'
         | 'V' | 'W' | 'X' | 'Y' | 'Z'

alpha ::= alphaU | alphaL

uint ::= digit{digit}
number ::= ['-'] uint | ([uint]'.'uint)

string ::= '\''{AnyCharacter}'\''
id ::= alpha{alpha | digit | '_'}

# eg. id <- 1
assignment ::= id "<-" expr

# last command in lambda is return
# (x y z) {
#		doubleX <- mul(x 2)
# 	mul(add(doubleX y) z)
# }
# #function above will return (2*x + y)*z 
lambdaDef ::= '(' {id} ')' 
	'{' { command ';'} '}'
# also can be used as array or whatever
# [1 2 3]
# [_ false]
# [1 true]
tuple ::= '[' {expr} ']'

namedTupleField ::= id '=' expr
# [id = 1 value = valueById(1)]
namedTuple ::= '[' {namedTupleField} ']'
namedTupleAcess ::= id ':' id

# match [v1 [v2 v3]] {
# 	[1 [2 3]] -> true;
# 	[2 [1 _]] -> false;
# 	[3 _] -> "3";
# 	_ -> NULL;
# }
patternMatching ::= 'match' expr '{'
	{expr '->' expr ';'}
	'_' '->' expr ';' '}'

# fun(arg1 a2)
functionCall ::= id '(' {expr} ')'

expr ::= number | string | id | functionCall | '_' 
       | "NULL" | namedTupleAcess | lambdaDef | namedTuple | tuple
command ::= assignment | functionCall | patternMatching
program ::= { command ';'}
```

## Commands
Basically programm consists of 1 or more commands,
which can be either assignment, function call or pattern matching.
Return value of the function call and pattern matching is ommited in this case.

### Assignment
Assignment (actually more like binding) syntax
consists of left hand side (identifier), and right hand side -
expressions that will be evaluated and identifier will refer to it.
```
someVariable <- someExpression;
```

### Function call
Function call consists of the same building blocks:
first we have an identifier, which has to refer
to some function (internal or previously defined by user),
then in parenthesis we have a list of expressions, that will be evaluated
and provided to function.

```
someFunction(someArg1 someFunction2(someArg2) 3);
```

### Pattern matching
Pattern matching is a bit more complex,
but is very simillar to switch statement:
basically you have a switch and some cases as well as default one,
your switch is compared against cases and first one that matches returns
its right hand side, if none matched
then we use right hand side of the default case.
Matching is not implemented yet, but it will work in the following way:
if either switch or case is '_' (this is wildcard for pattern matching),
```
match _ {
	literallyAnything -> someExpr;# this case will match
	...
};

match literallyAnything {
	_ -> someExpr;# this case will match as well
};
```
then the case matches, otherwise we have to check if they are of the same type:
if not - pattern doesn't match, if yes we match the values
stored inside the types in the following way:
if it's string or number we just check if they are equal,
```
match add(2 2) {
	add(3 1) -> someExpr;# this will match because both calls return number 4
	...
};

match 'hello' {
	'hello' -> someExpr;# this will match
};
```
if it's tuple we check if they have the same number of fields,
then recursively check if all the fields match.
```
match [v1 add(3 4) 'test'] {
	[v1 7 'test'] -> someExpr;# will match
};

match [a b c] {
	[a b] -> someExpr;# wont match 
}

match [a b c] {
	[a b _] -> someExpr;# will match 
}
```

for named tuple the process is simillar,
but cases might have less fields then the switch,
so we only match against the fields that case has,
if case has some field that switch doesn't - pattern match fails.
```
match [f1=a f2=c f3=whatever f4=_] {
	[f1=_ f4='test'] -> someExpr;# this will match because a and _ match,
	# as well as _ and 'test'
}
```

## Expressions
Expressions were mostly explained in previous part,
but we will go trough them again.

### Numbers
First are numbers, here are some examples:
```
1
0
-1
0.1
-0.1
-.1
```

### Strings
Then strings - basically whatever sequence of characters enclosed
in single quotes.

### Identifiers
Identifiers start with any letter,
and can contain letters numbers or underscore.

### Function call
Function call can also be an expression.

### Wildcard
Pattern match wild card is used only in pattern matching,
but you can use it for whatever.

### Null
Also we have NULL value which is returned from functions
that don't have any meaningful return value, for example print.

### Named tuple field acess
You can also acess fields of your named tuple like so:
```
a <- [f='test' f2=add(3 4)];
b <- a:f2;# b is now 7
```

### lambdas
This language doesn't have functions, but it has lambda
which are basically first class functions, here is an example:
```
myFunction <- (arg1 arg2) {
	add(arg1 arg2);
};
```
the last command in function returns the value,
so if we call our function like so:
```
myFunction(3 2);# this will return 5
```

### tuple
Tuple basically stores whatever list of values you give it.

### named tuple
Named tuple stores these values with some names, for ease of access.