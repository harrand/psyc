# Psyc

Psyc is a compiler for the Psy Programming Language.

## Support & Dependencies
Psyc currently only supports x86_64 AVX2 little-endian CPUs on any platform running under the linux-kernel. The compiler is hilariously incomplete: Optimisations, floating-point and SSE operations are not yet implemented.

Psyc has zero dependencies.

## Usage
`psyc [options] file.psy`
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

This compiler is clanker-free. This means that no form of AI assistance was used.

## Planned Work
- Optimisation passes
- Floating point operations
- AVX512 (optional, but on by default)
- AArch64

# Psy Programming Language <a name="intro"></a>

Psy is a highly-opinionated, low-level systems programming language.

### Dependency-Free
Psy programs, by design, are dependency-free and fully self-contained.
- It is impossible to produce a Psy program that statically links against anything.
- It is impossible to produce a Psy program that relies on a userland dynamic linker.
- Psy compilers may only produce non-PIE executables. They can *only* produce executables, this is by-design.
- Batteries not included. There is no standard libary or interop with libc. Infact, there is no runtime at all - you are likely to need to write your own entry-point.

As a result, Psy programs default to being very small, fast to compile, and userland-agnostic; a linux build will run on any platform that uses the linux kernel (from the past decade or two).

### Low-level
The language does not intend to insulate you from how your machine actually works, even if doing so could in theory improve optimisations. See examples below:
- Memory is memory; you are free to type-pun and cast pointers freely to your hearts content, so long as you don't directly access out-of-bounds of the memory.
- No overloads or redefining of language concepts. If you add two numbers, the compiler should emit some `add` instruction based on your target architecture, and never anything else.
- Stuff like division-by-zero, signed overflow are all implementation-defined. You are encouraged to rely on whatever your supported architectures/CPU manuals say what will happen in these cases.
- Compiler intrinsics are not offered, you are instead encouraged to write inline-assembly for all the targets you support.

## Anatomy of a Psy Program
Psy programs are simpler than C programs. A psy program consists of one or more build files, and any number of source files.
- Source files contain program code to be compiled.
- A build file is a source file that contains at least one build region.

A Psy program is simply the sum of all of its types/functions/assemblies/globals/build regions.

### Build Regions
Build regions are blocks of Psy code that contain build instructions. These are not compiled into machine code, but are interpreted at compile-time to control compilation. In other words, a build region replaces your build script.

Any source file that contains at least one build region is considered a build file. Because a program is comprised of at least one build file, all programs *must* contain *at least one* build region.

Build instructions within a region are pretty versatile. They can:
- Add source files, or add whole directories of source files.
- Invoke another *build region*, either in the same build file or in another.
- Specify the name of the output executable (by default, this is `build/psyout`).
- Specify which optimisations should be performed by the code generator.
- Do conditional logic, for example you can only add a source file if you are on windows, or fail compilation with a custom error message on ARM.
- Invoke external commands as a prebuild or postbuild command (though this is generally discouraged)
- Describe which function should be considered an entrypoint to the program (by default, this is called `main`)
