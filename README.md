# Psyc

Psyc is a compiler for the Psy Programming Language.

## Support & Dependencies
Psyc currently only supports x86_64 AVX2 little-endian CPUs on any platform running under the linux-kernel. It is not production-ready.

Psyc has zero dependencies.

## Usage
`psyc [options] file`
### Options
`-b`                    Specify custom build-directory (default: .`/build`)

`--dump-tokens`         Dump all tokens of all source files to stdout.

`--dump-ast`            Dump the full AST for each source file to stdout.

`--dump-ir`             Dump the IR instructions for all functions to stdout.

`--dump-ir=<name>`      Dump the IR instructios for function '<name>' to stdout.

`--dump-asm`            Dump the assembly instructions for all functions to stdout.

`--dump-asm=<name>`     Dump the assembly instructios for function '<name>' to stdout.

`--dump-metrics`        Dump code metrics for all functions (+ total) to stdout.

`--dump-metrics=<name>` Dump code metrics for function '<name>' to stdout.

`--skip-ir`             Skip the IR generation process, running only the frontend.

`--skip-asm`            Generate the IR, but don't lower it to assembly.

`--skip-object-write`   Generate the assembly, but don't write it to an executable.

## How it Works
1. You invoke `psyc my_build_file.psy`
2. Psyc reads the contents of the file, and passes the whole thing through a table-based finite-state-machine. A large array of tokens is created.
3. These tokens are parsed into a custom AST format via a handwritten LR(1) parser using a open-addressed hashtable.
4. After the AST is generated, treewalking occurs. This involes walking through the AST, searching for enums, structs, functions, assemblies and global variables. Each of these is recorded in a single, giant, whole-program-state.
5. The file provided to the compiler is a build file. This means its *build region* is executed immediately after treewalking. This could add more source/build files to the program. For each of these, steps 2-4 is repeated.
6. After the main build file is parsed, its *build region* is executed at compile-time. For any new source file added by the build region, step 2 and 3 is repeated (recursively). Afterwards, there is an array containing a fully-parsed AST for each source file in the program. Once all files in the project have been processed into the single whole-program-state, the rest of the compiler now works only on that state.
7. Semantic analysis is performed once, on the whole program state. This largely boils down to type-checking and preparing the program state for code generation.
8. Code generation involves generating a custom internal IR (psyir) using the verified whole program state. Most optimisations are performed on this IR, unless optimisations are disabled.
9. Afterwards, the whole program IR is lowered down to x86_64 assembly instructions (using a custom internal ABI). The assembly is optimised further, then encoded into machine code and the final executable is written out. Position-independent-executables (PIE) are not supported.
10. After that, postbuild commands are invoked and then the compiler exits, printing a basic summary of how much time was spent on each compilation stage.

This compiler is clanker-free. This means that no form of AI assistance was used, and any potential future AI-assisted contributions will be discarded with prejudice.

## Planned Work
- AArch64
- AVX512 (optional, but on by default)
- Large amounts of optimisation work, though matching LLVM is unrealistic.

# Psy Programming Language <a name="intro"></a>

Psy is a highly-opinionated, low-level systems programming language.

## Design Philosophy
Psy is designed to be somewhere between writing in your current machines assembly language, and writing in C. It is intended to be the best systems language for ignoring the past 50 years of software and reinventing the wheel.

### Autarkic Programs
Psy programs, by design, are dependency-free and fully self-contained.
- It is impossible to produce a Psy program that statically links against anything.
- It is impossible to produce a Psy program that relies on a userland dynamic linker.
- Psy compilers may only produce non-PIE, static executables.
- Batteries not included. There is no standard libary or interop with libc. Infact, there is no runtime at all.

As a result, Psy programs default to being very small, fast to compile, and userland-agnostic.

This is particularly great on Linux, because we can ignore the complete dumpster-fire that is the userland of every crappy linux distro, and just ship the same binary for all of them.

### Low-level
The language is not designed to insulate you from how your machine actually works. It also doesn't sacrifice user-developer experience in pursuit of optimisation wins.
- Memory is memory; you are free to type-pun and cast pointers freely to your hearts content, even if it leads to you carelessly stepping on a rake.
- There are no type-aliasing restrictions in Psy, nor any pointer provenance rules.
- No overloads or redefining of language concepts. If you add two numbers, the compiler should emit some `add` instruction based on your target architecture, and never anything else.
- Stuff like division-by-zero, signed overflow are all implementation-defined. You are encouraged to rely on whatever your supported architectures/CPU manuals say what will happen in these cases.

## Anatomy of a Psy Program
Psy programs are simpler than C programs. They are:
- Comprised of a single build file.
	- A build file is a source file that contains at least one *build region*. A *build region* is where your build script logic lives. It can do many things, but more on that later.
- Source files contain function definitions, type declarations (enums and structs), inline-assembly definitions (similar to LLVM-IR InlineAsm), and global variables.
- The order of declarations in source files does not matter. There is no need to pre-declare anything, and you can add your source files in any order you wish

### Build Regions
Build regions can:
- Add source files, or add whole directories of source files.
- Invoke another *build region*, either in the same build file or in another.
- Specify the name of the output executable.
- Specify whether debugging symbols should be written into the output executable.
- Specify which optimisations should be performed by the code generator.
- Do conditional logic, for example you can only add a source file if you are on windows, or fail compilation with a custom error message on ARM.
- Invoke external commands as a prebuild or postbuild command (though this is generally discouraged)
- Describe which function should be considered an entrypoint to the program (by default, this is called `main`)
