# Psy Language Reference

# Table of Contents
0. [Programs](#programs)
1. [Functions](#functions)
	- [Defining basic functions](#basic_functions)
2. [Types](#types)
	- [Type Qualifiers](#type_qualifiers)
	- [Primitive Types](#type_prim)
	- [Pointer Types](#type_ptr)
	- [Array Types](#type_array)
	- [Enum Types](#type_enum)
	- [Struct Types](#type_struct)
	- [Type Conversions](#type_conv)
		- [Primitive Conversions](#type_conv_prim)
		- [Struct Conversions](#type_conv_struct)
		- [Enum Conversions](#type_conv_enum)
   		- [Pointer Conversions](#type_conv_ptr)
       	- [Type Casting](#type_cast)
3. [Values](#values)
   	- [Literal Values](#val_lit)
   	- [Named Values](#val_named)
4. [Variables](#vars)
   	- [Global Variables](#var_glob)
   	- [Local Variables](#var_loc)
5. [Statements](#stmt)
   	- [Declaration Statements](#stmt_decl)
   	- [Expression Statements](#stmt_expr)
   	- [Return Statements](#stmt_ret)
   	- [Block Statements](#stmt_blk)
   	- [Designator Statements](#stmt_decl)
   	- [Metaregion Statements](#stmt_metaregion)
6. [Build Regions](#region)

## Programs <a name="programs"></a>

There are no concept of translation units in Psy. Your project, and all the files it comprises, are a part of a single program:
- Order of declarations is unimportant. You can call a function, and not declare it until later - so long as the declaration is ultimately in the program somewhere.
  	- This is also the case for structs, enums and macros.
- A program has exactly one build file - that is, a .psy file that contains the source code of the program.
	- You don't have to put the entire program's source code in this single file however. Using a build region, you can add additional source files to your program.
   		- The order in which you add additional source files does not matter. These source files can rely on the build file, or the other way around, or each other in any kind of order.
       	- The build region is also responsible for telling the compiler basic information about your program, such as:
      		- Any libraries you link against. There is no restriction on this, but you should ensure they are ABI-compatible with your choice of linker/target.
       	  	- The name of your output file (e.g 'foo.exe')
       	  	- The type of your output file. This can be either an executable, or a library or a single object file containing the entire program's source code.
       	  	- Optimisation level and build config flags.
       	  	- For details, see [Build Regions](#region).

## Functions <a name="functions"></a>

Exactly what you'd expect from other systems languages. A function consists of:
- A name
- Zero or more parameters (default parameter values are not supported), each with their own name and type.
- A return type.

### Defining basic functions <a name="basic_functions"></a>
The following code defines a function named `my_function_name`. It takes no parameters, and returns nothing (`v0` is the equivalent of `void` in C). Note that the return type is within the parentheses - this is a syntactical choice that seems odd at first. In actuality, it removes the necessity for arcane syntax for function pointers, but more on that later.
```
my_function_name ::= func(-> v0)
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
double_value ::= func(number : s32 -> s32)
{
	return number + number;
};
```

The following function doubles the value `5` and stores it in a new variable called `result`:
```
result ::= double_value(5);
```

## Types <a name="types"></a>
Psy is a statically-typed and strongly-typed language. This means that:
- The type of all variables are known by the compiler at compile-time.
- Typing rules are strict. Unlike languages such as C, implicit conversions are disabled by default - you must explicitly opt-into this.
  	- This means for example that a `u32` is not implicitly convertible to a `s64` or even a `s32`.

There are a small handful of type qualifiers available in Psy. Learn what these are first, or you will run into endless issues and confusion coming from C.

### Type Qualifiers <a name="type_qualifiers"></a>
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

 ### Primitive Types <a name="type_prim"></a>
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

### Pointer Types <a name="type_ptr"></a>
Pointers work almost identically to C pointers, but the syntax is slightly different. The best way to explain pointers is by example:
```
main ::= func(-> s32)
{
	my_value : s64 mut := 5;
	my_pointer : s64 mut& := ref my_value;

	// equivalent to: my_value = 0;
	(deref my_pointer) = 0;
	return 0;
};
```
Within a typename, pointer-ness is represented by the ampersand `&` symbol. It directly proceeds the base type representing the pointee.

- The `ref` keyword is equivalent to the 'address-of' operator (&) in C. `ref x` in Psy is equivalent to `&x` in C.
- Similarly, the `deref` keyword is equivalent to the 'dereference' operator (*) in C. `deref my_ptr` in Psy is equivalent to `*my_ptr` in C.
- Both `ref` and `deref` operators are examples of *unary operators*. These are operators that only require a single operand. More on that later.

Like C, you can also have function pointers:
```
// normal function definition
my_cool_function ::= func(-> v0)
{
	// code...
};

// later on in main:
main ::= func() -> s32 weak
{
	// function pointer variable.
	my_function_pointer : func(-> v0) := my_cool_function;
	// you can let the compiler determine the type for you:
	the_same_function_pointer ::= my_cool_function;
	my_function_pointer(); // calls my_cool_function.
};
```

### Array Types <a name="type_array"></a>
I consider arrays in C to be highly error-prone, particularly around its implicit conversion to a pointer (decay). Here's how it works in Psy:
```
// mutable array of three mutable u64s. initial values are undefined.
my_favourite_numbers : u64 mut[3] mut;
// pointer to first number (note that this does *not* perform a load, unlike dereferencing in C. this is pointer arithmetic)
pointer : u64& := my_favourite_numbers at 0;

// note that array does not implicitly decay to pointer
//another_pointer : u64& := my_favourite_numbers; // error!

// populate each value.
(deref pointer) = 7; // equivalent to (deref (my_favourite_numbers at 0)) = 7;
(deref (my_favourite_numbers at 1)) = 69;
(deref (my_favourite_numbers at 2)) = 420;
(deref (my_favourite_numbers at 0)) = zero;
// Array is now: 0, 69, 420

// write directly to the array.
my_favourite_numbers = u64[3]{0; 1; 2;};
// Array is now: 0, 1, 2
my_favourite_numbers = zero;
// Array is now: 0, 0, 0
```

### Enum Types <a name="type_enum"></a>
Enums in Psy are similar to `enum class` in C++11. The syntax is slightly different.
```
window_flags ::= enum
{
	.none := 0x0000;
	.opengl := 0x0001;
	.vulkan := 0x0002;
};

// later on in a function:
my_flag : window_flags := window_flags.opengl;
value ::= my_flag@s64; // 1
```
Enums can convert *only* to `s64` to mitigate loss of precision. Two convert to different integer types you will have to convert to `s64` initially i.e `window_flags.opengl@s64@u8`

### Struct Types <a name="type_struct"></a>
Structs in Psy are virtually identical to that of C. Structs must be defined as new types, and then can be used as a type for variables. A syntax very similar to C/C++20 designated initialisers can be used to initialise a struct value.

#### Declaring a new struct
```
my_struct ::= struct
{
	// data members go here.
	my_data_member : s32;
};
```
- You cannot pre-declare structs, as there is no need to do so.
- Recursive-structs are illegal. That is, `mystruct::data_member` can never be of type `mystruct`, including arrays and pointers.

The syntax for creating a variable of a struct type is intuitive and similar to that of C:
```
myvar1 : my_struct mut;
// note that setting data members after initialisation like this requires the variable to be mutable.
myvar1.my_data_member = 5;

// struct initialiser:
myvar2 : my_struct := my_struct
{
	.my_data_member := 5;
};
```

#### Struct Initialisers
Struct initialisers are very similar to C++20 designated initializers. It is the best way to initialise multiple data members of a new struct value at once, as opposed to setting them manually.
- Unlike C++20, you do not have to list the initialisers in-order.
- It is valid to not initialise every single data member of the struct. However, the data members you don't set in the struct initialiser will be of indeterminate value. It is considered *erroneous behaviour* to read indeterminate values.

### Type Conversions <a name="type_conv"></a>
Without the `weak` qualifier, almost no implicit type conversions are available to you.
```
my_int : u64 := 5;
my_int2 : s64 := my_int; // error.
```
If either type `A` or `B` are `weak`, then the following type conversion rules are in effect:
#### Primitive Conversions <a name="type_conv_prim"></a>
- If A and B are *numeric primitives*, conversion is allowed.
- If A and B are `s64` and an *enum* respectively, then conversion is allowed (and vice-versa).
- If A is a `bool` and B is a *numeric primitive*, conversion is allowed. The same is true in reverse.
- If A is a `v0`, no form of type conversion is allowed.
#### Struct Conversions <a name="type_conv_struct"></a>
- Struct types do not convert to anything else.
#### Enum Conversions <a name="type_conv_enum"></a>
- If A is an enum type and B is `s64`, then conversion is allowed.
- Enums do not convert to anything else.
#### Pointer Conversions <a name="type_conv_ptr"></a>
- Pointer types can be freely converted to other pointer types, with the following restriction:
	- You cannot apply mutability with a cast. `T&` cannot be converted to `T mut&`
 - While `*reinterpret_cast<T*>(some_pointer)` is almost always "undefined behaviour" in C, it is not so in Psy. There are no aliasing rules in place, so you are free to do this - memory is memory and there is no type-based alias analysis.

### Type Casting <a name="type_cast"></a>
You have now learned about the various possible type conversions. No form of conversion is done without the `weak` qualifier. This is so that these conversions don't happen unless you ask for them - an attempt to produce more predictable code. However, if you want to opt-into these conversions, then it should be easy.

Casting is the act of explicitly applying `weak`ness to a value's type, and allowing it to undergo a conversion. It is done using the `@` symbol. It is a different approach to C and C++. Here's a very basic example:
```
my_int : u64 := 5;
// my_int2 : s64 := my_int; // you haven't asked for implicit conversions, this is an error.
my_int2 : s64 := my_int@s64; // ok. you have casted my_int to s64. my_int2 == 5;
```
In this case, `my_int@s64` is a binary operator (the casting operator). On the left of the `@` symbol is the value you want to convert. On the right of the symbol is the typename you want to convert to.
The equivalent in C would be:
```c
uint64_t my_int = (uint64_t)5;
int64_t my_int2 = (int64_t)my_int;
```
Note that in this C example the casts are unnecessary, as these types convert implicitly already.

I wanted to provide an additional quality-of-life compromise for those who like to opt into these conversions. Instead of having to `@type_name_goes_here` every single time, you have a few further options:
```
my_int : u64 weak := 5;
my_int2 : s64 := my_int; // ok. my_int was explicitly defined with the weak qualifier, this variable will continually opt into implicit conversions.
```
What if you want to "apply weakness" to an existing variable? You can do so using this very arcane syntax:
```
my_int : u64 := 5;
my_int@_ // this is the same as my_int@u64 weak. or in general, value@_ is equivalent to value@type_of_value weak
```

This will allow you to be very selective in where you opt-into implicit conversions, and makes it easy to do so when you don't need the very strict type safety rules.
```
my_int2 : s64 := my_int@_;
// my_int remains a strongly-typed u64. the "apply weakness" idiom only affects the expression it is used in -- it doesnt magically change the type qualifier of the variable forevermore.
```


# Values <a name="values"></a>
Values represent a stored permutation of bits, representative of a given type. Unlike C, values of a given type have no such concept of a lifetime - only memory has a lifetime. For this reason, the notion of constructors and destructors do not exist in Psy. Values are used to initialise variables, re-assign mutable variables, and pass/return from functions.

## Literal Values <a name="val_lit"></a>
Literal values are values restricted to certain types, and are known at compile time. Literal values can be used to initialise `static` variables.
`5` is a literal value, and so is `"my awesome string literal!"`. `my_function()` does not yield a literal value, as functions compute their values at runtime.

Struct initialisers can be literal values, but only if every initialiser is a literal value. For example, the following struct initialiser is a literal value:
```
animal
{
	.type := animal_type.tiger;
	.name := "bob";
};
```

As such, it can be used to initialise a `animal static`:
```
bob_the_tiger : animal static := animal
{
	.type := animal_type.tiger;
	.name := "bob";
};
```

The following struct initialiser is not a literal value, as one of its initialisers is the result of a non-static function call:
```
new_world ::= world
{
	.name := "My New World";
	.seed := generate_random_seed();
};
world_cpy : world static := new_world; // error: cannot convert world to world static.
```

## Named Values <a name="val_named"></a>
Named Values (otherwise known as lvalues in C) are values that have an identifiable location in memory. All variables are named values.

# Variables <a name="vars"></a>
Variables in Psy are very similar to other C-like languages. Variables are named values that are of a given type.

# Global Variables <a name="var_glob"></a>
Global variables are variables whose memory is not automatically reserved - the lifetime of its memory matches that of the program's runtime. Global variables do not have to be initialised initially, but if they are given an initialiser, that initialiser must be a literal value.

Global variables are visible across the whole of the source file, aswell as any source files that add the containing source file in its *build metaregion*.

# Local Variables <a name="var_loc"></a>
Local variables are variables that are limited to a scope. Their lifetime semantics exactly match that of C.

# Statements <a name="stmt"></a>
Statements represent the main building blocks of a Psy program. Each statement compiles down to a sequential list of zero or more instructions.

Some statements are purely a compile-time mechanism and do not generate any code.

## Declaration Statements <a name="stmt_decl"></a>
Declaration statements are the primary way to define variables. The syntax of a declaration statement is as follows:
```
(1)
decl_name : typename;
(2)
decl_name : typename := init_expr;
(3)
decl_name ::= init_expr;
(4)
decl_name ::= funcdef_expr
{
	...
};
(5)
decl_name ::= funcdef_expr := extern;
```
The behaviour of (1) is as follows:
* Allocate memory on the stack, enough to store a value of type `typename`.
* If this statement is within a function implementation block, then this is a local variable, and the variable `decl_name` is now available to proceeding code within the block.
  	* If it is outside of a function implementation block and a top-level statement within the source file, then it represents a global variable. The variable `decl_name` is now available to all proceeding code in the source file, aswell as any code that adds this source file in its *build metaregion*.
* The variable has an indeterminate value.
Example:
```
myvar : u32;
```

The behaviour of (2) is identical to (1), except that the expression `init_expr` is treated as an initialiser. The variable now has that value.
Example:
```
myvar : u32 := 500;
```

The behaviour of (3) is similar to (2), except that the type of the variable is deduced automatically by the `init_expr` initialiser. The type of the variable will exactly match that of the initialiser, including *type qualifiers*.
Example:
```
myvar ::= 500;
// note: myvar is of type (s64 static weak)
```

Both (4) and (5) are only used to declare *functions*.
(4) Declares a function, and provides an implementation block for the function.
Example:
```
main ::= func(-> s32 weak)
{
	hello_world();
	return 0;
};
```

(5) Declares a function, but indicates that the implementation of the function lives elsewhere. The linker is expected to locate this implementation after compilation. You should use this to declare functions from other libraries (.lib/.o) that you wish to link against and call in your program.
Example:
```
wglGetProcAddress ::= func(unnamedParam1 : u8& -> u64 weak) := extern;
```

# Build Regions <a name="region"></a>
Build regions are a special syntax within a .psy file that contain directives for the compiler regarding building the final program. Here is an example build region:

```
// a.psy
== default ==
{
	set_executable("foo");
	set_optimization(3);
	run_command("echo building foo...");
	add_source_file("foo.psy");

	static if(_win32)
	{
		add_link_library("User32.lib");
	}
}
```

Note that this is not a function - its purely a compile-time set of instructions that the compiler will use to inform its behaviour. This region is named 'default'.

This means that, given:
`psyc a.psy -b default`
Then this region will be interpreted by the compiler. Note that 'default' is a special name - if no region name is specified via the `-b` flag then 'default' will be used. In this case, `psyc a.psy` works the same. When ran, this region means that:
- The output will be an executable named 'foo'. The file extension is chosen automatically by the compiler - a sane default depending on your platform (e.g .exe for windows, .out for linux).
- The optimization level is 3, which is the equivalent of `-O3` for other compilers, i.e maximum optimization.
- During the build, before the output file is generated, the command `echo building foo...` will be invoked in a shell and its output is fed through to stdout.
- The file `foo.psy` will be compiled.
- If the host process is running on Windows, then:
	- The final executable `foo.exe` will link against `User32.lib`.

Important Note: `add_source_file` will not invoke any build regions within the file that you added. To do that, you will want the `add_build_file` directive.
