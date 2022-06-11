# fluent
This an interpreter for dynamically typed language 'fluent'.
Currently it only supports a subset of the language, but it allows for some simple programs.

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