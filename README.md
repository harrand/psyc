# Psy Language Cheatsheet

## Functions

Exactly what you'd expect from other systems languages. A function consists of:
- A name
- Zero or more parameters (default parameter values are not supported), each with their own name and type.
- A return type.

## Defining basic functions
The following code defines a function named `my_function_name`. It takes no parameters, and returns nothing (`v0` is the equivalent of `void` in C).
```
my_function_name ::= func() -> v0
{
	// code goes here
};
```
Calling the function is just like you would expect:
```
my_function_name();
```
Note that you cannot call a function within the global scope. That is - you can only call a function within another function body.
In a function that returns `v0`, there is no need to return at the end of the function. In all other cases, you *must* return a value.

The following code defines a function named `double_value`. It takes a single parameter, doubles it, and returns the result.
```
double_value ::= func(number : s32) -> s32
{
	return number + number;
};
```

The following function doubles the value `5` and stores it in a new variable called `result`:
```
result ::= double_value(5);
```

## Types
Psy is a statically-typed and strongly-typed language. This means that:
- The type of all variables are known at compile-time.
- Typing rules are strict. Unlike languages such as C, implicit conversions are disabled by default - you must explicitly opt-into this.
  	- This means for example that a `u32` is not implicitly convertible to a `s64` or even a `s32`.

 ### Primitive Types
 There are a number of primitive types:
 | Psy Type         | C Equivalent | Description                            |
| :---------------- | :----------: | :------------------------------------- |
| s64               |   int64_t    | 64-bit signed integer.                 |
| s32               |   int32_t    | 32-bit signed integer.                 |
| s16               |  int16_t     | 16-bit signed integer.                 |
| s8                |  int8_t      | 8-bit signed integer.                  |
| u64               |  uint64_t    | 64-bit unsigned integer.               |
| u32               |  uint32_t    | 32-bit unsigned integer.               |
| u16               |  uint16_t    | 16-bit unsigned integer.               |
| u8                |  uint8_t     | 8-bit unsigned integer.                |
| bool              |  BOOL        | 1-bit `true`/`false` value.            |
| f64               |  double      | 64-bit floating-point number. IEEE-754 |
| f32               |  float       | 32-bit floating-point number. IEEE-754 |
| v0                |  void        | Represents no value. Zero size.        |
