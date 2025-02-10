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

There are a small handful of type qualifiers available in Psy. Learn what these are first, or you will run into endless issues and confusion coming from C.

### Type Qualifiers
A type can have zero or more qualifiers. Qualifiers appear at the end of the type's name.
| Type Qualifier         | C Equivalent      | Explanation                                                                    |
| :--------------------- | :---------------: | :----------------------------------------------------------------------------- |
| none                   | `const`           | Everything is immutable by default, like Rust but unlike C.                    |
| `mut`                  |                   | The variable is mutable - it's value can be changed after initialisation.      |
| `weak`                 | none              | Types that are marked `weak` will be subject to *implicit conversions*.        |
| `static`               | `constexpr` (C++) | The variable must be a compile-time constant, or a compile error will occur.   |

Note that a type can have multiple qualifiers. Here are some examples of various types:
- `u64` - immutable, no implicit conversions, not a compile-time constant.
- `f32 mut` - mutable, no implicit conversions, not a compile-time constant.
- `v0& weak static` - pointer type. implicit conversions allowed. compile-time constant. pointee is `v0` - immutable, no implicit conversions, not a compile-time constant.

 ### Primitive Types
 There are a number of primitive types:
 | Primitive Type         | C Equivalent | Description                            |
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

### Pointer Types
Pointers work almost identically to C pointers, but the syntax is slightly different. The best way to explain pointers is by example:
```
main ::= func() -> v0
{
	my_value : s64 mut := 5;
	my_pointer : s64 mut& := ref my_value;

	// equivalent to: my_value = 0;
	(deref my_pointer) = 0;
};
```
Within a typename, pointer-ness is represented by the ampersand `&` symbol. It directly proceeds the base type representing the pointee.

- The `ref` keyword is equivalent to the 'address-of' operator (&) in C. `ref x` in Psy is equivalent to `&x` in C.
- Similarly, the `deref` keyword is equivalent to the 'dereference' operator (*) in C. `deref my_ptr` in Psy is equivalent to `*my_ptr` in C.
- Both `ref` and `deref` operators are examples of *unary operators*. These are operators that only require a single operand. More on that later.

Like C, you can also have function pointers. Also like C, the syntax is a little (albeit less) arcane. A quick example is below, but I will go into detail later:
```
// normal function definition
my_cool_function ::= func() -> v0
{
	// code...
};

// later on in main:
main ::= func() -> s32 weak
{
	// function pointer variable.
	my_function_pointer : func() -> v0 := my_cool_function;
	// you can let the compiler determine the type for you:
	the_same_function_pointer ::= my_cool_function;
	my_function_pointer(); // calls my_cool_function.
};
```

### Array Types
I consider arrays in C to be highly error-prone, particularly around its implicit conversion to a pointer (decay). Here's how it works in Psy:
```
	// array of three u64s. initial values are undefined.
	my_favourite_numbers : u64#3;
	// pointer to first number (note that this does *not* perform a load, unlike dereferencing in C. this is pointer arithmetic)
	pointer : u64& := my_favourite_numbers at 0;

	// note that array does not implicitly decay to pointer
	//another_pointer : u64& := my_favourite_numbers; // error!

	// populate each value.
	(deref pointer) = 7; // equivalent to (deref (my_favourite_numbers at 0)) = 7;
	(deref (my_favourite_numbers at 1)) = 69;
	(deref (my_favourite_numbers at 12)) = 420;

	// Array is now: 7, 69, 420.
```
