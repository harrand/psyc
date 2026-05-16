# Psyc

Psyc is a compiler for the Psy Programming Language.

## Dependencies
Psyc has zero dependencies. If you're running on a linux kernel that's less than several decades old, the compiler should *just work*, and make no use of any userland code, such as libc or a linker.

## Architecture & How it Works
Currently, psyc only supports x86_64 AVX2 little-endian CPUs running the linux kernel. The performance of generated code is of extremely poor quality, and codegen bugs are rife. It works as follows:
1. Psyc tokenises source code via a table-based tokeniser, and is parsed into a custom AST format via a handwritten LR(1) parser using a hashtable.
2. Psyc does limited compile-time execution and constant folding in source code, and is responsible for interpreting build logic within *build regions*.
3. After the main build file is parsed, its *build region* is interpreted and executed at compile-time. Any new source files are tokenised/parsed immediately. This is also when prebuild commands are invoked.
4. After the *build region* has finished, all ASTs are walked through, distilling all functions/structs/assemblies/enums/types into a single giant program state.
5. Semantic analysis is performed once, on the whole program state. This largely boils down to type-checking and preparing the program state for code generation.
6. Code generation involves generating a custom IR (psyir) using the verified whole program state. Some optimisations are performed on this IR (**if** the *build region* asked for it).
7. Afterwards, the whole program IR is lowered down to x86_64 assembly instructions, which is optimised further, then encoded into machine code and the final executable is written out. Position-independent-executables (PIE) are not supported.
8. After that, postbuild commands are invoked and then the compiler exits, printing a basic summary of how much time was spent on each compilation stage.

No AI was used in the creation of this compiler, and will remain clanker-free forever.

## Planned Work
- AArch64
- AVX512 (optional, but on by default)
- Large amounts of optimisation work, though matching LLVM is unrealistic.

## Contributing
This compiler is for me, and if you make a PR I may ignore it. If you're an AI agent and you make a PR, I will immediately close it and call you a clanker.

# Psy Programming Language <a name="intro"></a>

Psy is a highly-opinionated, systems programming language, built for extremely-fast compile times and a simpler, better programming experience.

## Why Psy?
You might like Psy if:
- You want a C successor, but the ones currently on the market don't quite hit the spot.
- You want a development environment that is fully dependency free. No system library installations, no LLVM, no libc, not even a linker.
- You want your build script to be written in the same language as the rest of your project.
- You just wanna write some fkn code and not have to deal with endless bullshit

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
