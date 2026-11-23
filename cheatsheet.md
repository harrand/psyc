# Cheatsheet

## Syntax

### Variable Declaration


```go

my_var1 : the_type; // undefined value
my_var2 : the_type := the_init_value; // well-defined init value, explicitly-typed
my_var3 ::= the_init_value; // well-defined init value, type exactly matches that of the init value
```

### Function Declaration
> **_NOTE:_**  You must declare a function only *once* throughout the entire program. Pre-declarations are unnecessary.

```go

my_func_name : func(param0 : type0, param1 : type1 -> return_type)
{
	// function body goes here
};
```

## String Literals


String literals in Sy are not nul-terminated by default. If you want a nul-terminating string, you must add the terminator yourself. For example:

```go

my_assert(my_zstrlen("foo") == 3); // false
my_assert(my_zstrlen("foo\0") == 3); // true
```

## Foo

Bar baz.
