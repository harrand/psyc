# Cheatsheet

## Syntax

### Variable Declaration
- Variables can be declared at top-level scope, which makes them a global variable which lives in global data.
- Variables declared inside of function bodies are local variables that live on the stack.
- Variables declared inside of a build region are compile-time constants that are not visible in the rest of the programs code, unless you expose them via the build-builtin `$declare(name_of_the_variable)`

```go

		my_var1 : the_type; // undefined value
		my_var2 : the_type := the_init_value; // well-defined init value, explicitly-typed
		my_var3 ::= the_init_value; // well-defined init value, type exactly matches that of the init value
```

### Function Declaration
- Functions with a body must always be declared at top-level scope.
- Functions without a body are simply variables (i.e a function pointer).
> **_NOTE:_**  You must declare a function only *once* throughout the entire program. Pre-declarations are unnecessary.

```go

		// function declaration of 'my_func_name'
		my_func_name : func(param0 : type0, param1 : type1 -> return_type)
		{
			// function pointer local variable. zero-initialised (better not call it!)
			my_function_ptr : func(-> v0) := zero;
		};
```
### Integer Literals

Integer literals have intuitive syntax. They are of type `s64 weak`. Example:
```go

		my_favourite_number : s64 weak := 862;
```
### Floating-point Literals

Floating-point literals have intuitive syntax. They are of type `f64 weak`. Example:
```go

		the_funny_number : f64 weak := 69.420;
```

### String Literals


String literals in Sy are not nul-terminated by default. If you want a nul-terminating string, you must add the terminator yourself. For example:

```go

		my_assert(my_zstrlen("foo") == 3); // false
		my_assert(my_zstrlen("foo\0") == 3); // true
```

## Types
Sy is a strongly-typed language, because data type rules are strictly enforced. All implicit conversions are banned by default, though there is a type qualifier `weak` you can use to opt-into some.

A type consists of a base type (this is either a primitive, enum, struct, array or pointer) and zero or more qualifiers.

### Enum Types

Enum types represent a fixed-size set of integral constants. All enum types are compiled down to 64-bit signed integers (i.e `s64` under-the-hood). An example enum declaration might look like this:

```go

		colour : enum
		{
			.blue := 0;
			.red := 1;
			.green := 2;
		};

		my_favourite_colour ::= colour.red; // type: colour
```

### Struct Types
Struct types represent a group of related data types into one place for easy usage. Memory layout rules of a struct are ABI-specific, but they are largely identical to that of C-based languages. An example struct declaration might look like this:

```go

		wallet : struct
		{
			total_pennies : u64;
			card_count : u64;
			card_values : u64 mut?;
			appearance : colour;
		};

		my_wallet ::= wallet
		{
			.total_pennies := 10000;
			.card_count := 0;
			.card_values := zero;
			.appearance := colour.green;
		};
```

### Array Types
Array types consist of a base-type (which could itself be an array type) followed by an array length. The syntax is extremely similar to C-based languages. An example array might look like this:

```go

		fibonacci_numbers : u64 mut[8];
		[fibonacci_numbers # 0] = 1;
		[fibonacci_numbers # 1] = 1;
		[fibonacci_numbers # 2] = 2;
		// and so on...
```
### Primitive Types

| Sy Type  | C Equivalent  |
| -------- | ------- |
| `v0`       | `void`    |
| `u64`      | `uint64_t`|
| `u32`      | `uint32_t`|
| `u16`      | `uint16_t`|
| `u8`       | `uint8_t` |
| `s64`      | `sint64_t`|
| `s32`      | `sint32_t`|
| `s16`      | `sint16_t`|
| `s8`       | `sint8_t` |
| `f64`      | `double`  |
| `f32`      | `float`   |
| `bool`     | `bool`    |

- Variable-sized integer types do not exist in Sy.
- `bool` values are guaranteed to be 1-byte in size on all platforms.

### Type Qualifiers

Any type can have any permutation/combination of qualifiers.

| Sy Qualifier  | Description | C Equivalent  |
| -------- | ------- | ------- |
| `mut`      | value can be changed (mutable)    | not-`const` (everything is const by default)
| `static`   | value is a compile-time constant (statically-known) | `constexpr`
| `weak`     | type undergoes implicit conversions | N/A (all types in C are equivalent to `weak`)
| abi_sysv | function type will use the System V ABI | N/A

Some example of qualified types look like:
```go

		a : u64 mut;
		b : u64 mut weak?;
		b : u8 mut? mut? mut;
		s : s64 static;
```
