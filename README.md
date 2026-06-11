# Psyc

Psyc is a compiler for the Psy Programming Language.

## Dependencies
Psyc has zero dependencies. If you're running on a linux kernel that's less than several decades old, the compiler should *just work*, and make no use of any userland code, such as libc or a linker.

## Architecture & How it Works
Currently, psyc only supports x86_64 AVX2 little-endian CPUs running the linux kernel. The performance of generated code is of extremely poor quality, and codegen bugs are rife. It works as follows:
1. You invoke `psyc my_build_file.psy`
2. Psyc reads the contents of the file, and passes the whole thing through a table-based finite-state-machine. A large array of tokens is created. These tokens are parsed into a custom AST format via a handwritten LR(1) parser using a open-addressed hashtable.
3. After the main build file is parsed, its *build region* is executed at compile-time. For any new source file added by the build region, step 2 and 3 is repeated (recursively). Afterwards, there is an array containing a fully-parsed AST for each source file in the program.
4. Program-walking aka 'progwalking' occurs. All ASTs are walked through, distilling all functions, inline-assemblies, globals, and types into a single, giant whole program state.
5. Semantic analysis is performed once, on the whole program state. This largely boils down to type-checking and preparing the program state for code generation.
6. Code generation involves generating a custom internal IR (psyir) using the verified whole program state. Most optimisations are performed on this IR, unless optimisations are disabled.
7. Afterwards, the whole program IR is lowered down to x86_64 assembly instructions (using a custom internal ABI). The assembly is optimised further, then encoded into machine code and the final executable is written out. Position-independent-executables (PIE) are not supported.
8. After that, postbuild commands are invoked and then the compiler exits, printing a basic summary of how much time was spent on each compilation stage.

No AI was used in the creation of this compiler, and will remain clanker-free forever.
**Please note that psyc does not currently have a license. This means that it is open-source insofar as you can view the code, but beware that draconian default copyright laws apply until I add a license.**

## Planned Work
- AArch64
- AVX512 (optional, but on by default)
- Large amounts of optimisation work, though matching LLVM is unrealistic.
- Add a license. Until there is an anti-AI license that I am happy with, it will remain without one.

# Unplanned/Rejected Work
- LLVM support. I have removed this and am *never* going back
- LSP/other tooling support
- Foreign Function Interface (FFI)
- Static Linking

# Psy Programming Language <a name="intro"></a>

Psy is a highly-opinionated, systems programming language, built for extremely-fast compile times and simple programs.

## Why Psy?
You might like Psy if:
- You want a C successor, but the ones currently on the market don't quite hit the spot.
- You want a development environment that is fully dependency free. No system library installations, no LLVM, no libc, not even a linker.
- You trust yourself to create your software, and want minimal hand-holding.

## Why not Psy?
You may *not* like Psy if:
- You rely on other peoples code, and don't want to use a tool that prevents you from doing so.
- You want the compiler to perform correctness and safety checks for you (beyond type-checking).
- You require highly-optimised code generation, such as LLVM.
- You are a proponent of any of the following:
	- Objects (classes, constructors and destructors, OOP, etc...)
	- Garbage collection (GC)
	- Resource Acquisition is Initialisation (RAII)
	- Macros
	- Header files
	- Having to pre-declare, or even think about the order of your declarations in your pgoram.

## Anatomy of a Psy Program
Psy programs are simpler than C programs. They are:
- Comprised of a single build file.
	- A build file is a source file that contains at least one *build region*. A *build region* is where your build script logic lives. It can do many things, but more on that later.
- Source files contain function definitions, type declarations (enums and structs), inline-assembly definitions (similar to LLVM-IR InlineAsm), and global variables.
- The order of declarations in source files does not matter. There is no need to pre-declare anything, and you can add your source files in any order you wish

## Build Regions
Build regions can:
- Add source files, or add whole directories of source files.
- Invoke another *build region*, either in the same build file or in another.
- Specify the name of the output executable.
- Specify whether debugging symbols should be written into the output executable.
- Specify which optimisations should be performed by the code generator.
- Do conditional logic, for example you can only add a source file if you are on windows, or fail compilation with a custom error message on ARM.
- Invoke external commands as a prebuild or postbuild command (though this is generally discouraged)
- Describe which function should be considered an entrypoint to the program (by default, this is called `main`)
